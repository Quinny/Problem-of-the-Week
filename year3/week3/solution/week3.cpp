#include <cmath>
#include <iostream>
#include <unordered_map>
#include <vector>

// Type aliases to save my wrists and improve readability. Win win.

template <typename T>
using Matrix = std::vector<std::vector<T>>;

template <typename T>
using Histogram = std::unordered_map<T, std::size_t>;

// Given a collection of items, return a histogram of the occurances of each
// item.
template <typename T>
Histogram<T> make_histogram(const std::vector<T>& items) {
  Histogram<T> ret;
  for (const auto& item : items) {
    ++ret[item];
  }
  return ret;
}

// Represents a corpus of labeled documents.  A document is defined as a vector
// of Terms.  A label is a unique identifier for a document.
template <typename Label, typename Term = std::string>
class Corpus {
 public:
  // Each row in the term matrix should correspond to a single document.  Row
  // "i" of the term matrix will be labeled by element "i" of the label vector.
  Corpus(const std::vector<Label>& document_labels,
         const Matrix<Term>& term_matrix)
      : document_labels_(document_labels) {
    for (const auto& term_vector : term_matrix) {
      documents_.push_back(make_histogram(term_vector));
    }
  }

  // Return the tfidf score of the query for each document in the corpus.
  std::vector<std::pair<Label, double>> tfidf(const Term& query) {
    std::size_t documents_containing_query = 0;

    std::vector<std::pair<Label, double>> scored_documents;
    for (auto document_index = 0UL; document_index < documents_.size();
         ++document_index) {
      auto check = documents_[document_index].find(query);
      documents_containing_query += check != documents_[document_index].end();
      double tf = check == documents_[document_index].end() ? 0 : check->second;
      scored_documents.emplace_back(document_labels_[document_index], tf);
    }

    double idf =
        documents_containing_query == 0
            ? 0
            : std::log10(documents_.size() /
                         static_cast<double>(documents_containing_query));
    for (auto& scored_document : scored_documents) {
      scored_document.second *= idf;
    }
    return scored_documents;
  }

 private:
  std::vector<Label> document_labels_;
  std::vector<Histogram<Term>> documents_;
};

int main() {
  Matrix<std::string> documents{{"this", "sentence", "is", "about", "ducks"},
                                {"this", "sentence", "is", "about", "dogs"},
                                {"dogs", "dogs", "dogs"}};
  std::vector<int> labels{1, 2, 3};

  Corpus<int> c(labels, documents);
  auto scores = c.tfidf("dogs");

  for (const auto& scored_document : scores) {
    std::cout << scored_document.first << " " << scored_document.second
              << std::endl;
  }
}
