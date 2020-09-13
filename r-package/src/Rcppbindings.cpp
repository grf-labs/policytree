/*-------------------------------------------------------------------------------
  This file is part of policytree.

  policytree is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  policytree is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with policytree. If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------*/
#include <queue>
#include <Rcpp.h>

#include "tree_search.h"

/**
  * Find the depth `depth` tree that maximizes the sum of rewards.
  *
  * @param X The features
  * @param Y The rewards
  * @param depth The tree depth (0-indexed). An integer greater than or equal to zero.
  * @param split_step The number of possible splits to consider when performing tree search.
  * (an integer greater than or equal to one.)
  * @return The best tree stored in an adjacency list (same format as `grf`).
  *
  * The returned list's first entry:
  * a list of of lists (nodes), where each each leaf node
  * have the entries: ($is_leaf = TRUE, $action = column_id_of_best_action)
  * and each non-leaf node have entries:
  * ($is_leaf = FALSE, split_variable = colum_id_of_best_split,
  *  $split_value = best_split, $left_child = list_index_of_left_child,
  *  $right_child = list_index_of_right_child).
  * The returned list's second entry:
  * The same tree represented as an array for faster lookups. We return the
  * first representation for seamless integration with GRF, which uses the same
  * data structure.
  */
// [[Rcpp::export]]
Rcpp::List tree_search_rcpp(const Rcpp::NumericMatrix& X,
                            const Rcpp::NumericMatrix& Y,
                            int depth,
                            int split_step) {
  size_t num_rows = X.rows();
  size_t num_cols_x = X.cols();
  size_t num_cols_y = Y.cols();
  const Data* data = new Data(X.begin(), Y.begin(), num_rows, num_cols_x, num_cols_y);

  std::unique_ptr<Node> root = tree_search(depth, split_step, data);

  // We store the tree as the same list data structure (`nodes`) as GRF for seamless integration with
  // the plot and print methods. We also store the tree as an array (`tree_array`) for faster lookups.
  // This will make a difference for a very large amount of lookups, like n = 1 000 000.
  // The columns 0 to 3 are:
  // split_variable (-1 if leaf) | split_value (action_id if leaf) | left_child | right_child
  int num_nodes = pow(2.0, depth + 1.0) - 1;
  Rcpp::NumericMatrix tree_array(num_nodes, 4);
  Rcpp::List nodes;
  int i = 1;
  int j = 0;
  std::queue<std::unique_ptr<Node>> frontier;
  frontier.push(std::move(root));
  while (frontier.size() > 0) {
    auto node = std::move(frontier.front());
    frontier.pop();
    if (node->left_child == nullptr && node->right_child == nullptr) {
      auto list_node = Rcpp::List::create(Rcpp::Named("is_leaf") = true,
                                          Rcpp::Named("action") = node->action_id + 1); // C++ index
      nodes.push_back(list_node);
      tree_array(j, 0) = -1;
      tree_array(j, 1) = node->action_id + 1;
    } else {
      auto list_node = Rcpp::List::create(Rcpp::Named("is_leaf") = false,
                                          Rcpp::Named("split_variable") = node->index + 1, // C++ index
                                          Rcpp::Named("split_value") = node->value,
                                          Rcpp::Named("left_child") = i + 1,
                                          Rcpp::Named("right_child") = i + 2);
      nodes.push_back(list_node);
      tree_array(j, 0) = node->index + 1;
      tree_array(j, 1) = node->value;
      tree_array(j, 2) = i + 1; // left child
      tree_array(j, 3) = i + 2; // right child
      frontier.push(std::move(node->left_child));
      frontier.push(std::move(node->right_child));
      i += 2;
    }
    j++;
  }
  Rcpp::List result;
  result.push_back(nodes);
  result.push_back(tree_array);

  delete data;
  return result;
}

/**
  * Return the action index for query samples.
  *
  * @param tree_array The tree.
  * @param X The query samples.
  * @return A vector of action IDs.
  *
  */
// [[Rcpp::export]]
Rcpp::NumericVector tree_search_rcpp_predict(const Rcpp::NumericMatrix& tree_array,
                                             const Rcpp::NumericMatrix& X) {
  size_t num_samples = X.rows();
  Rcpp::NumericVector result(num_samples);
  for (size_t sample = 0; sample < num_samples; sample++) {
    size_t node = 0;
    while (true) {
      bool is_leaf = tree_array(node, 0) == -1;
      if (is_leaf) {
        size_t action = tree_array(node, 1);
        result(sample) = action;
        break;
      }
      size_t split_var = tree_array(node, 0) - 1; // Offset by 1 for C++ indexing
      double split_value = tree_array(node, 1);
      double value = X(sample, split_var);
      if (value <= split_value) {
        node = tree_array(node, 2) - 1;
      } else {
        node = tree_array(node, 3) - 1;
      }
    }
  }

  return result;
}
