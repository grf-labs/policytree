---
header-includes:
  - \usepackage[linesnumbered,ruled]{algorithm2e}
title: 'policytree: Policy learning via doubly robust empirical welfare maximization over trees'
tags:
  - R
  - causal inference
  - econometrics
authors:
  - name: Erik Sverdrup
    affiliation: 1
    orcid: 0000-0001-6093-1390
  - name: Ayush Kanodia
    affiliation: 1
  - name: Zhengyuan Zhou
    affiliation: 2
  - name: Susan Athey
    affiliation: 1
  - name: Stefan Wager
    affiliation: 1
affiliations:
 - name: Stanford Graduate School of Business
   index: 1
 - name: NYU Stern
   index: 2
date: 20 January 2020
bibliography: paper.bib
---

# Summary

The problem of learning treatment assignment policies from randomized or observational data arises in many fields. For example, in personalized medicine, we seek to map patient observables (like age, gender, heart pressure, etc.) to a treatment choice using a data-driven rule.

There has recently been a considerable amount of work on statistical methodology for policy learning, including @manski2004statistical, @zhao2012estimating, @swaminathan2015batch, @kitagawa2018should, @mbakop2016model, @athey2017efficient, @kallus2018confounding and @zhou2018offline. In particular, @kitagawa2018should show that if we only consider policies $\pi$ restricted to a class $\Pi$ with finite VC dimension and have access to data from a randomized trial with $n$ samples, then an empirical welfare maximization algorithm achieves regret that scales as $\sqrt{\operatorname{VC}(\Pi) / n}$. @athey2017efficient extend this result to observational studies via doubly robust scoring, and @zhou2018offline further consider the case with multiple treatment choices (in particular, the regret will depend on the tree depth, feature space, and number of actions).

The package ``policytree`` for @R implements the multi-action doubly robust approach of @zhou2018offline in the case where we want to learn policies $\pi$ that belong to the class $\Pi$ of depth-$k$ decision trees. In order to use ``policytree``, the user starts by specifying a set of doubly robust scores for policy evaluation; the software then carries out globally optimal weighted search over decision trees.

It is well known that finding an optimal tree of arbitrary depth is NP-hard. However, if we restrict our attention to trees of depth $k$, then the problem can be solved in polynomial time. Here, we implement the global optimization via an exhaustive (unconstrained) tree search that runs in $O(P^{k} N^{k} (\log N + D) + PN\log N)$ time, where $N$ is the number of individuals, $P$ the number of characteristics observed for each individual and $D$ is the number of available treatment choices (see details below). If an individual’s characteristics only takes on a few discrete values, the runtime can be reduced by a factor of $N^k$. Additionally, an optional approximation parameter lets the user control how many splits to consider.

![A depth 2 tree fit on data from the National Job Training Partnership Act Study [@bloom1997benefits]. The reward matrix contains two outcomes: not assigning treatment (action 1), and assigning treatment, a job training program (action 2). The covariate matrix contains two variables: a candidate's previous annual earnings in \$1,000 and years of education.](example.png)

Our package is integrated with the ``R`` package ``grf`` of @athey2019generalized, allowing for a simple workflow that uses random forests to estimate the nuisance components required to automatically form the doubly robust scores. We also generalize the ``causal_forest`` function from ``grf`` to multiple treatment effects with a one vs all encoding described in @zhou2018offline. The following simulation example illustrates this workflow in a setting with $D = 3$ actions; here, we write covariates with `X`, outcomes as `Y`, and actions as `W`. Figure 1 shows a tree similarly grown on a dataset considered by @kitagawa2018should.

```R
library(policytree)
X <- matrix(rnorm(2000 * 10), 2000, 10)
W <- sample(c("A", "B", "C"), 2000, replace = TRUE)
Y <- X[,1] + X[,2] * (W == "B") + X[,3] * (W == "C") + runif(2000)
multi.forest <- multi_causal_forest(X = X, Y = Y, W = W)
DR.scores <- double_robust_scores(multi.forest)
tr <- policy_tree(X, DR.scores, depth = 2)
plot(tr)
```

The core tree search functionality is built in C++ using the ``Rcpp`` interface [@eddelbuettel2011rcpp].
This approach to tree search is discussed further by @zhou2018offline, who find it to scale better to large sample size problems than an alternative based on mixed-integer programming. We also note an existing ``R`` package, ``evtree`` by @JSSv061i01, which can be used to heuristically optimize over decision trees via evolutionary search.

# Appendix:

### Details on tree search

The pseudocode for the tree search is outlined in Algorithm 1 and Algorithm 2. At a high level, in the main recursive case for $k >= 2$, the algorithm maintains the data structure $sorted\_sets$ to quickly obtain the sort order of points along all dimensions $P$ for a given split. For each of the $P \times (N - 1)$ possible splits, for each dimension j all points on the right side are stored in   $set_R(j)$. All points on the left side are stored in $set_L(j)$. For each split candidate, the point is moved from the right set to the left set for all dimensions. This proceeds recursively to enumerate the reward in all possible splits.

The $O(PN\log N)$ term arises from the fixed amortized cost of creating the global sort order once for every sample along all P dimensions. The remaining $O(P^{k} N^{k} (\log N + D))$ term is obtained by inductively calculating the runtime for increasing depths $k$.

<!-- \pagebreak -->

\begin{algorithm}[H]
  \captionsetup{singlelinecheck=off}
  \caption[]{Exact tree search.\\
  	In the implementation, parents with identical actions in both leaves are \textit{pruned}. It also features an optional approximation parameter than controls the number of splits to consider.\\
  	The recursion base case is both at a leaf node ($k=0$) as well as at the parent of a leaf ($k=1$) where one can jointly
    compute the best action in each leaf in $O(NPD)$ by a dynamic programming style algorithm). Peripheral functions are outlined at the end}
  \SetKwInOut{Input}{Input}
  \SetKwInOut{Output}{Output}

  \underline{function tree\_search} $(sorted\_sets, \Gamma, k)$\;
  \Input{$P$-vector $sorted\_sets$, $N \times D$ score matrix $\Gamma$, tree depth $k$}
  \Output{The optimal tree, a structure with (left node, right node, total reward, action)}

  \If{$k$ = 0}{
    $tree.reward, tree.action$ $\leftarrow$ $\{$max, argmax$\}_{j \in 1,...,d} \sum_{i \in 1,...,N} \Gamma_{ij}$\;
    $tree.left = \emptyset, tree.right = \emptyset$ \;
    return $tree$\;
  }

  \If{$k$ = 1} {
    return $tree\_search\_single\_split(sorted\_sets, \Gamma)$\;
  }

  $best\_tree_L$ $\leftarrow$ $\emptyset$\;
  $best\_tree_R$ $\leftarrow$ $\emptyset$\;
  $best\_reward \leftarrow -\infty$\;

  \For{p=1:P}{
    $sets_R$ $\leftarrow$ $copy(sorted\_sets)$\;
    $sets_L$ $\leftarrow$ $create\_empty\_sorted\_sets()$\;
    \For{n=(1: N-1)}{
      $sample_n$ $\leftarrow$ $sets_R(p).begin()$\;
      $sets_L(p).insert(sample_n)$\;
      $sets_R(p).erase(sample_n)$\;

      \For{$j \neq p$}{
        $set_R(j).erase(sample_n)$\;
        $set_L(j).insert(sample_n)$\;
      }

      $tree_L$ $\leftarrow$ tree\_search$(sets_L, \Gamma, k - 1)$\;
      $tree_R$ $\leftarrow$ tree\_search$(sets_R, \Gamma, k - 1)$\;
      $reward = tree_L.reward + tree_R.reward$\;

      \If{$best\_tree\_L = \emptyset$ $||$ $reward > best\_reward$}{
            $best\_tree_L \leftarrow tree_L$\;
            $best\_tree_R \leftarrow tree_R$\;
            $best\_reward = reward$\;
      }
    }
  }
  return $tree(best\_tree_L, best\_tree_R, best\_reward, \emptyset)$\;
\end{algorithm}

\pagebreak

\begin{algorithm}
  \caption{O(NPD) implementation for a single split}
  \SetKwInOut{Input}{Input}
  \SetKwInOut{Output}{Output}

  \underline{function tree\_search\_single\_split} $(sorted\_sets, \Gamma)$\;
  \Input{$P$-vector $sorted\_sets$, $N \times D$ score matrix $\Gamma$}
  \Output{The optimal tree, a structure with (left node, right node, total reward, action)}

  $cum\_rewards \leftarrow array(D)(P)(N)$ \;
  \For{$d=(1: D)$}{
    \For{$p=(1: P)$}{
      $iter = sorted\_sets(p).first()$\;
      $index = iter.index()$\;
      $iter.next()$\;
      $cum\_rewards(d)(p)(1) \leftarrow \Gamma_{1 index}$\;
      \For{$n=(2: N)$}{
        $index = iter.index()$\;
        $iter.next()$\;
        $cum\_rewards(d)(p)(n) \leftarrow \Gamma_{index d} + cum\_rewards(d)(p)(n-1)$\;
      }
    }
  }
  $best\_reward_L, best\_reward_R \leftarrow \emptyset, \emptyset$\;
  $best\_action_L, best\_action_R \leftarrow \emptyset, \emptyset$\;
  \For{$p=(1: P)$}{
    \For{$n=(1: N)$}{
      $ reward_L, action_L \leftarrow $ \linebreak
      $\{$max, argmax$\}_{d \in 1,..,D} cum\_rewards(d)(p)(n)$\;
      $ reward_R, action_R \leftarrow $ \linebreak
      $\{$max, argmax$\}_{d \in 1,..,D} cum\_rewards(D)(p)(N) - cum\_rewards(d)(p)(n)$\;
    }
    \If{$reward_L + reward_R > best\_reward_L + best\_reward_R$}{
      $best\_reward_L, best\_action_L \leftarrow reward_L, action_L$\;
      $best\_reward_R, best\_action_R \leftarrow reward_R, action_R$\;
    }
  }
  $best\_tree_L \leftarrow tree(\emptyset, \emptyset, best\_reward_L, best\_action_L)$\;
  $best\_tree_R \leftarrow tree(\emptyset, \emptyset, best\_reward_R, best\_action_R)$\;
  return $tree(best\_tree_L, best\_tree_R, best\_reward_L + best\_reward_R, \emptyset)$\;
\end{algorithm}

### Deriving the running time
\textbf{Base Case 1}: k = 0 (no splits): In this case, all we need to do is calculate the
sum of rewards over each of the available treatment choices D for the N users.
Hence, the time complexity is $O(ND)$.

\textbf{Base Case 2}: k = 1 (1 split): In this case, as we show in Algorithm 2,
the time complexity is $O(NPD + NP \log N)$.  We first sort all N points along all P dimensions.
This accounts for the $NP \log N$ term. Along each dimension, first we keep a cumulative
sum of rewards for each treatment on both sides of every possible split. This
takes time $O(ND)$ given the sorted order on points along that dimension. We can then
calculate the best split point given this sort order, along with the best policy in both splits in
time $O(ND)$, as in the pseudocode. Doing this over all dimensions, we get
$O(NPD)$. Combining this with the initial sort, we get $O(NPD + NP \log N) = O(NP(\log N + D))$.

\textbf{Recursive Case} We propose the time complexity for $k >= 1$ (1 or more
splits) to be $O(P^{k} N^{k} (\log N + D))$. This is satisfied for base case
2 above. For the recursive case, there are $PN$ possible split points.
For every single split along along every dimension
we remove a sample from a Binary Search Tree and
add to another; this takes $O(\log N)$ time, and we do this for each of the $P$
dimensions, leading to time $(P \log N)$. Further, for each split, we
recursively call $tree\_search$ for depth $k - 1$, in general there
are $m_1$ and $m_2$ points in each split at
the top level such that $N = m_1 + m_2$.
Assuming the recursive expression, the
amount of work done for each split is then

$O(P \log N + m_1^{k-1} P^{k-1} (\log m_1 + D) + m_2^{k-1} P^{k-1} (\log m_2 + D))$

Note that,

$m_1^{k-1} P^{k-1} (\log {m_1} + D) < m_1^{k-1} P^{k-1}(\log N + D)$ since $m_1 < N$.

Similarly,

$m_2^{k-1}P^{k-1}(\log m_2 + D) < m_2^{k-1}P^{k-1}(\log N + D)$ since $m_2 < N$.

Further,

$m_1^{k-1} P (\log N + D) + m_2^{k-1} P (\log N + D) < N^{k-1} P^{k-1} (\log N + D)$

since $m_1 + m_2 = N, m_1, m_2, N > 0$.

Combining, the amount of work in each split is upper bounded by

$O(P^{k-1} N^{k-1} (\log N + d))$.

Since we have $P N$ splits, this leads to a running time of

$O(PN (P^{k-1} N^{k-1} (\log N + d)) = O(P^{k}N^{k} (\log N + d))$.

<!-- \pagebreak -->

<!-- \pagebreak -->

\begin{algorithm}[H]
  \renewcommand{\thealgocf}{}
  \caption{Peripheral functions for Algorithm 1}
  \SetKwInOut{Input}{Input}
  \SetKwInOut{Output}{Output}

  \underline{function create\_sorted\_sets} $(X)$\;
  \Input{$N \times P$ covariate matrix $X$}
  \Output{A length $P$ vector, the jth vector containing all $N$ samples sorted along dimension $j$}

  result $\leftarrow$ vector(P)\;
  \For{j=1:P}{
    $result(j)$ $\leftarrow$ binary\_search\_tree(j)\;
      \For{i=1:N}{
        $result(j)$.insert($x_i$)\;
      }
  }
  return result\;

  \SetKwInOut{Input}{Input}
  \SetKwInOut{Output}{Output}

  \underline{function create\_empty\_sorted\_sets} $()$\;
  \Input{$P$ Number of dimensions}
  \Output{A length $P$ vector, the jth vector is empty, but to be sorted along dimension $j$}

  result $\leftarrow$ vector(P)\;
  \For{j=1:P}{
    $result(j)$ $\leftarrow$ binary\_search\_tree(j)\;
  }
  return result\;
\end{algorithm}

\pagebreak

# References
