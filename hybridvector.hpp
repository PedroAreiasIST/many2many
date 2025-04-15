#ifndef HYBRID_VECTOR_HPP
#define HYBRID_VECTOR_HPP
#include "smallvector.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <vector>

template <typename T, size_t SizeThreshold = 64> class hybrid_vector {
private:
  // Storage strategy selection
  using storage_type =
      std::conditional_t<(sizeof(T) <= SizeThreshold),
                         T,    // Direct storage for small objects
                         T *>; // Pointer storage for large objects

  // The actual storage container
  smallvector<storage_type> data_;

  // Helper to determine if we're using pointer storage
  static constexpr bool uses_pointers = (sizeof(T) > SizeThreshold);

  // Helper for value access
  T &get_value(size_t pos) {
    if constexpr (uses_pointers) {
      return *(data_[pos]);
    } else {
      return data_[pos];
    }
  }

  const T &get_value(size_t pos) const {
    if constexpr (uses_pointers) {
      return *(data_[pos]);
    } else {
      return data_[pos];
    }
  }

  // Helper to create storage element from value
  storage_type create_storage(const T &value) {
    if constexpr (uses_pointers) {
      return new T(value);
    } else {
      return value;
    }
  }

  storage_type create_storage(T &&value) {
    if constexpr (uses_pointers) {
      return new T(std::move(value));
    } else {
      return std::move(value);
    }
  }

  // Helper to clean up storage
  void delete_storage(storage_type &elem) {
    if constexpr (uses_pointers) {
      delete elem;
      elem = nullptr;
    }
  }

  // Helper to copy storage
  storage_type copy_storage(const storage_type &other) {
    if constexpr (uses_pointers) {
      return other ? new T(*other) : nullptr;
    } else {
      return other;
    }
  }

public:
  // Type definitions
  using value_type = T;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using size_type = typename smallvector<storage_type>::size_type;
  using difference_type = typename smallvector<storage_type>::difference_type;

  // Iterator implementation
  template <bool IsConst> class iterator_impl {
  private:
    using parent_type =
        std::conditional_t<IsConst, const hybrid_vector *, hybrid_vector *>;
    using iterator_type =
        std::conditional_t<IsConst,
                           typename smallvector<storage_type>::const_iterator,
                           typename smallvector<storage_type>::iterator>;

    parent_type parent_;
    iterator_type it_;

  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = T;
    using difference_type = typename smallvector<storage_type>::difference_type;
    using pointer = std::conditional_t<IsConst, const T *, T *>;
    using reference = std::conditional_t<IsConst, const T &, T &>;

    iterator_impl() : parent_(nullptr) {}
    iterator_impl(parent_type parent, iterator_type it)
        : parent_(parent), it_(it) {}

    iterator_impl &operator++() {
      ++it_;
      return *this;
    }
    iterator_impl operator++(int) {
      auto tmp = *this;
      ++it_;
      return tmp;
    }
    iterator_impl &operator--() {
      --it_;
      return *this;
    }
    iterator_impl operator--(int) {
      auto tmp = *this;
      --it_;
      return tmp;
    }

    iterator_impl &operator+=(difference_type n) {
      it_ += n;
      return *this;
    }
    iterator_impl operator+(difference_type n) const {
      return iterator_impl(parent_, it_ + n);
    }
    iterator_impl &operator-=(difference_type n) {
      it_ -= n;
      return *this;
    }
    iterator_impl operator-(difference_type n) const {
      return iterator_impl(parent_, it_ - n);
    }

    difference_type operator-(const iterator_impl &other) const {
      return it_ - other.it_;
    }

    reference operator*() const {
      if constexpr (uses_pointers) {
        return *(*it_);
      } else {
        return *it_;
      }
    }

    pointer operator->() const {
      if constexpr (uses_pointers) {
        return *it_;
      } else {
        return &(*it_);
      }
    }

    reference operator[](difference_type n) const {
      if constexpr (uses_pointers) {
        return *(*(it_ + n));
      } else {
        return *(it_ + n);
      }
    }

    bool operator==(const iterator_impl &other) const {
      return it_ == other.it_;
    }
    bool operator!=(const iterator_impl &other) const {
      return it_ != other.it_;
    }
    bool operator<(const iterator_impl &other) const { return it_ < other.it_; }
    bool operator<=(const iterator_impl &other) const {
      return it_ <= other.it_;
    }
    bool operator>(const iterator_impl &other) const { return it_ > other.it_; }
    bool operator>=(const iterator_impl &other) const {
      return it_ >= other.it_;
    }

    friend iterator_impl operator+(difference_type n, const iterator_impl &it) {
      return it + n;
    }
  };

  using iterator = iterator_impl<false>;
  using const_iterator = iterator_impl<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  // Constructors and assignment operators

  // Default constructor
  hybrid_vector() = default;

  // Size constructor
  explicit hybrid_vector(size_type count) {
    if constexpr (uses_pointers) {
      data_.resize(count, nullptr);
      for (size_type i = 0; i < count; ++i) {
        data_[i] = new T();
      }
    } else {
      data_.resize(count);
    }
  }

  // Size and value constructor
  hybrid_vector(size_type count, const T &value) {
    if constexpr (uses_pointers) {
      data_.resize(count, nullptr);
      for (size_type i = 0; i < count; ++i) {
        data_[i] = new T(value);
      }
    } else {
      data_.resize(count, value);
    }
  }

  // Range constructor
  template <typename InputIt,
            typename = std::enable_if_t<!std::is_integral<InputIt>::value>>
  hybrid_vector(InputIt first, InputIt last) {
    if constexpr (uses_pointers) {
      for (auto it = first; it != last; ++it) {
        data_.push_back(new T(*it));
      }
    } else {
      data_.assign(first, last);
    }
  }

  // Copy constructor
  hybrid_vector(const hybrid_vector &other) {
    if constexpr (uses_pointers) {
      data_.reserve(other.size());
      for (const auto &elem : other.data_) {
        data_.push_back(elem ? new T(*elem) : nullptr);
      }
    } else {
      data_ = other.data_;
    }
  }

  // Move constructor
  hybrid_vector(hybrid_vector &&other) noexcept
      : data_(std::move(other.data_)) {
    // Moving the smallvector is enough, as it will take ownership of the
    // pointers
  }

  // Initializer list constructor
  hybrid_vector(std::initializer_list<T> init) {
    if constexpr (uses_pointers) {
      data_.reserve(init.size());
      for (const auto &value : init) {
        data_.push_back(new T(value));
      }
    } else {
      data_.assign(init);
    }
  }

  // Copy assignment
  hybrid_vector &operator=(const hybrid_vector &other) {
    if (this != &other) {
      if constexpr (uses_pointers) {
        // Clean up existing elements
        for (auto &elem : data_) {
          delete elem;
        }

        // Copy new elements
        data_.clear();
        data_.reserve(other.size());
        for (const auto &elem : other.data_) {
          data_.push_back(elem ? new T(*elem) : nullptr);
        }
      } else {
        data_ = other.data_;
      }
    }
    return *this;
  }

  // Move assignment
  hybrid_vector &operator=(hybrid_vector &&other) noexcept {
    if (this != &other) {
      if constexpr (uses_pointers) {
        // Clean up existing elements
        for (auto &elem : data_) {
          delete elem;
        }
      }
      data_ = std::move(other.data_);
    }
    return *this;
  }

  // Initializer list assignment
  hybrid_vector &operator=(std::initializer_list<T> init) {
    if constexpr (uses_pointers) {
      // Clean up existing elements
      for (auto &elem : data_) {
        delete elem;
      }

      // Assign new elements
      data_.clear();
      data_.reserve(init.size());
      for (const auto &value : init) {
        data_.push_back(new T(value));
      }
    } else {
      data_ = init;
    }
    return *this;
  }

  // Destructor
  ~hybrid_vector() {
    if constexpr (uses_pointers) {
      for (auto &elem : data_) {
        delete elem;
      }
    }
  }

  // Assign
  void assign(size_type count, const T &value) {
    if constexpr (uses_pointers) {
      // Clean up existing elements
      for (auto &elem : data_) {
        delete elem;
      }

      // Assign new elements
      data_.clear();
      data_.reserve(count);
      for (size_type i = 0; i < count; ++i) {
        data_.push_back(new T(value));
      }
    } else {
      data_.assign(count, value);
    }
  }

  template <typename InputIt,
            typename = std::enable_if_t<!std::is_integral<InputIt>::value>>
  void assign(InputIt first, InputIt last) {
    if constexpr (uses_pointers) {
      // Clean up existing elements
      for (auto &elem : data_) {
        delete elem;
      }

      // Assign new elements
      data_.clear();
      for (auto it = first; it != last; ++it) {
        data_.push_back(new T(*it));
      }
    } else {
      data_.assign(first, last);
    }
  }

  void assign(std::initializer_list<T> init) {
    if constexpr (uses_pointers) {
      // Clean up existing elements
      for (auto &elem : data_) {
        delete elem;
      }

      // Assign new elements
      data_.clear();
      data_.reserve(init.size());
      for (const auto &value : init) {
        data_.push_back(new T(value));
      }
    } else {
      data_.assign(init);
    }
  }

  // Element access
  reference at(size_type pos) {
    if (pos >= size()) {
      throw std::out_of_range("hybrid_vector::at - Index out of range");
    }
    return get_value(pos);
  }

  const_reference at(size_type pos) const {
    if (pos >= size()) {
      throw std::out_of_range("hybrid_vector::at - Index out of range");
    }
    return get_value(pos);
  }

  reference operator[](size_type pos) { return get_value(pos); }

  const_reference operator[](size_type pos) const { return get_value(pos); }

  reference front() { return get_value(0); }

  const_reference front() const { return get_value(0); }

  reference back() { return get_value(size() - 1); }

  const_reference back() const { return get_value(size() - 1); }

  // Iterators
  iterator begin() noexcept { return iterator(this, data_.begin()); }

  const_iterator begin() const noexcept {
    return const_iterator(this, data_.begin());
  }

  const_iterator cbegin() const noexcept {
    return const_iterator(this, data_.cbegin());
  }

  iterator end() noexcept { return iterator(this, data_.end()); }

  const_iterator end() const noexcept {
    return const_iterator(this, data_.end());
  }

  const_iterator cend() const noexcept {
    return const_iterator(this, data_.cend());
  }

  reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }

  const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }

  const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }

  reverse_iterator rend() noexcept { return reverse_iterator(begin()); }

  const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  // Capacity
  [[nodiscard]] bool empty() const noexcept { return data_.empty(); }

  size_type size() const noexcept { return data_.size(); }

  size_type max_size() const noexcept { return data_.max_size(); }

  void reserve(size_type new_cap) { data_.reserve(new_cap); }

  size_type capacity() const noexcept { return data_.capacity(); }

  void shrink_to_fit() { data_.shrink_to_fit(); }

  // Modifiers
  void clear() noexcept {
    if constexpr (uses_pointers) {
      for (auto &elem : data_) {
        delete elem;
      }
    }
    data_.clear();
  }

  iterator insert(const_iterator pos, const T &value) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      auto it = data_.insert(data_.begin() + index, new T(value));
      return iterator(this, it);
    } else {
      auto it = data_.insert(data_.begin() + index, value);
      return iterator(this, it);
    }
  }

  iterator insert(const_iterator pos, T &&value) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      auto it = data_.insert(data_.begin() + index, new T(std::move(value)));
      return iterator(this, it);
    } else {
      auto it = data_.insert(data_.begin() + index, std::move(value));
      return iterator(this, it);
    }
  }

  iterator insert(const_iterator pos, size_type count, const T &value) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      auto insertPos = data_.begin() + index;
      for (size_type i = 0; i < count; ++i) {
        insertPos = data_.insert(insertPos, new T(value)) + 1;
      }
      return iterator(this, data_.begin() + index);
    } else {
      auto it = data_.insert(data_.begin() + index, count, value);
      return iterator(this, it);
    }
  }

  template <typename InputIt,
            typename = std::enable_if_t<!std::is_integral<InputIt>::value>>
  iterator insert(const_iterator pos, InputIt first, InputIt last) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      smallvector<T *> temp;
      for (auto it = first; it != last; ++it) {
        temp.push_back(new T(*it));
      }
      auto insertPos = data_.begin() + index;
      for (auto it = temp.rbegin(); it != temp.rend(); ++it) {
        insertPos = data_.insert(insertPos, *it);
      }
      return iterator(this, data_.begin() + index);
    } else {
      auto it = data_.insert(data_.begin() + index, first, last);
      return iterator(this, it);
    }
  }

  iterator insert(const_iterator pos, std::initializer_list<T> ilist) {
    return insert(pos, ilist.begin(), ilist.end());
  }

  template <typename... Args>
  iterator emplace(const_iterator pos, Args &&...args) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      auto it = data_.insert(data_.begin() + index,
                             new T(std::forward<Args>(args)...));
      return iterator(this, it);
    } else {
      auto it =
          data_.emplace(data_.begin() + index, std::forward<Args>(args)...);
      return iterator(this, it);
    }
  }

  iterator erase(const_iterator pos) {
    auto index = pos - cbegin();
    if constexpr (uses_pointers) {
      delete data_[index];
    }
    auto it = data_.erase(data_.begin() + index);
    return iterator(this, it);
  }

  iterator erase(const_iterator first, const_iterator last) {
    auto start = first - cbegin();
    auto end = last - cbegin();

    if constexpr (uses_pointers) {
      for (auto i = start; i < end; ++i) {
        delete data_[i];
      }
    }

    auto it = data_.erase(data_.begin() + start, data_.begin() + end);
    return iterator(this, it);
  }

  void push_back(const T &value) {
    if constexpr (uses_pointers) {
      data_.push_back(new T(value));
    } else {
      data_.push_back(value);
    }
  }

  void push_back(T &&value) {
    if constexpr (uses_pointers) {
      data_.push_back(new T(std::move(value)));
    } else {
      data_.push_back(std::move(value));
    }
  }

  template <typename... Args> reference emplace_back(Args &&...args) {
    if constexpr (uses_pointers) {
      data_.push_back(new T(std::forward<Args>(args)...));
      return *data_.back();
    } else {
      data_.emplace_back(std::forward<Args>(args)...);
      return data_.back();
    }
  }

  void pop_back() {
    if (!empty()) {
      if constexpr (uses_pointers) {
        delete data_.back();
      }
      data_.pop_back();
    }
  }

  void resize(size_type count) {
    if (count < size()) {
      if constexpr (uses_pointers) {
        for (size_type i = count; i < size(); ++i) {
          delete data_[i];
        }
      }
      data_.resize(count);
    } else if (count > size()) {
      if constexpr (uses_pointers) {
        size_type oldSize = size();
        data_.resize(count, nullptr);
        for (size_type i = oldSize; i < count; ++i) {
          data_[i] = new T();
        }
      } else {
        data_.resize(count);
      }
    }
  }

  void resize(size_type count, const T &value) {
    if (count < size()) {
      if constexpr (uses_pointers) {
        for (size_type i = count; i < size(); ++i) {
          delete data_[i];
        }
      }
      data_.resize(count);
    } else if (count > size()) {
      if constexpr (uses_pointers) {
        size_type oldSize = size();
        data_.resize(count, nullptr);
        for (size_type i = oldSize; i < count; ++i) {
          data_[i] = new T(value);
        }
      } else {
        data_.resize(count, value);
      }
    }
  }

  void swap(hybrid_vector &other) noexcept { data_.swap(other.data_); }
};

// Non-member functions
template <typename T, size_t ST>
bool operator==(const hybrid_vector<T, ST> &lhs,
                const hybrid_vector<T, ST> &rhs) {
  if (lhs.size() != rhs.size()) {
    return false;
  }

  return std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

template <typename T, size_t ST>
bool operator!=(const hybrid_vector<T, ST> &lhs,
                const hybrid_vector<T, ST> &rhs) {
  return !(lhs == rhs);
}

template <typename T, size_t ST>
bool operator<(const hybrid_vector<T, ST> &lhs,
               const hybrid_vector<T, ST> &rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(),
                                      rhs.end());
}

template <typename T, size_t ST>
bool operator<=(const hybrid_vector<T, ST> &lhs,
                const hybrid_vector<T, ST> &rhs) {
  return !(rhs < lhs);
}

template <typename T, size_t ST>
bool operator>(const hybrid_vector<T, ST> &lhs,
               const hybrid_vector<T, ST> &rhs) {
  return rhs < lhs;
}

template <typename T, size_t ST>
bool operator>=(const hybrid_vector<T, ST> &lhs,
                const hybrid_vector<T, ST> &rhs) {
  return !(lhs < rhs);
}

// Specialized algorithms
template <typename T, size_t ST>
void swap(hybrid_vector<T, ST> &lhs, hybrid_vector<T, ST> &rhs) noexcept {
  lhs.swap(rhs);
}

#endif // HYBRID_VECTOR_HPP