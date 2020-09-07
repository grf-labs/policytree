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
  * The returned list: a list of of lists (nodes), where each each leaf node
  * have the entries: ($is_leaf = TRUE, $action = column_id_of_best_action)
  * and each non-leaf node have entries:
  * ($is_leaf = FALSE, split_variable = colum_id_of_best_split,
  *  $split_value = best_split, $left_child = list_index_of_left_child,
  *  $right_child = list_index_of_right_child).
  *
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

  Rcpp::List nodes;
  int i = 1;
  std::queue<std::unique_ptr<Node>> frontier;
  frontier.push(std::move(root));
  while (frontier.size() > 0) {
    auto node = std::move(frontier.front());
    frontier.pop();
    if (node->left_child == nullptr && node->right_child == nullptr) {
      auto list_node = Rcpp::List::create(Rcpp::Named("is_leaf") = true,
                                          Rcpp::Named("action") = node->action_id + 1); // C++ index
      nodes.push_back(list_node);
    } else {
      auto list_node = Rcpp::List::create(Rcpp::Named("is_leaf") = false,
                                          Rcpp::Named("split_variable") = node->index + 1, // C++ index
                                          Rcpp::Named("split_value") = node->value,
                                          Rcpp::Named("left_child") = i + 1,
                                          Rcpp::Named("right_child") = i + 2);
      nodes.push_back(list_node);
      frontier.push(std::move(node->left_child));
      frontier.push(std::move(node->right_child));
      i += 2;
    }
  }

  delete data;
  return nodes;
}

/**
  * Return the action index for query samples.
  *
  * @param nodes The tree.
  * @param X The query samples.
  * @return A vector of action IDs.
  *
  */
// [[Rcpp::export]]
Rcpp::NumericVector tree_search_rcpp_predict(const Rcpp::List& nodes,
                                             const Rcpp::NumericMatrix& X) {
  size_t num_samples = X.rows();
  Rcpp::NumericVector result(num_samples);
  for (size_t sample = 0; sample < num_samples; sample++) {
    size_t node = 0;
    while (true) {
      const Rcpp::List& list_node = nodes[node];
      bool is_leaf = list_node["is_leaf"];
      if (is_leaf) {
        size_t action = list_node["action"];
        result(sample) = action;
        break;
      }
      size_t split_var = list_node["split_variable"];
      split_var = split_var - 1; // Convert back to C++ index
      double split_value = list_node["split_value"];
      double value = X(sample, split_var);
      if (value <= split_value) {
        node = list_node["left_child"];
      } else {
        node = list_node["right_child"];
      }
      node = node - 1; // Convert back to C++ index
    }
  }

  return result;
}
