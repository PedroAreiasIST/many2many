#ifndef smallvector_HPP
#define smallvector_HPP

#include <algorithm>
#include <array>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <vector>

template <typename T, std::size_t N = 16> class smallvector {
private:
  // Internal storage
  std::array<T, N> small_buffer;
  std::vector<T> large_buffer;

  // Flag to indicate which buffer is active
  bool using_small_buffer;

  // Size tracking
  std::size_t curr_size;

  // Helper methods
  void switch_to_large_buffer() {
    if (using_small_buffer) {
      large_buffer.assign(small_buffer.begin(),
                          small_buffer.begin() + curr_size);
      for (std::size_t i = 0; i < curr_size; ++i) {
        small_buffer[i].~T();
      }
      using_small_buffer = false;
    }
  }

public:
  // Type definitions to match std::vector
  using value_type = T;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  // Custom iterator classes that delegate to either small_buffer or
  // large_buffer
  class iterator {
  private:
    smallvector *container;
    size_type index;

  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using pointer = T *;
    using reference = T &;

    iterator(smallvector *container, size_type index)
        : container(container), index(index) {}

    reference operator*() const { return (*container)[index]; }

    pointer operator->() const { return &((*container)[index]); }

    iterator &operator++() {
      ++index;
      return *this;
    }

    iterator operator++(int) {
      iterator tmp = *this;
      ++(*this);
      return tmp;
    }

    iterator &operator--() {
      --index;
      return *this;
    }

    iterator operator--(int) {
      iterator tmp = *this;
      --(*this);
      return tmp;
    }

    iterator &operator+=(difference_type n) {
      index += n;
      return *this;
    }

    iterator operator+(difference_type n) const {
      iterator tmp = *this;
      return tmp += n;
    }

    iterator &operator-=(difference_type n) {
      index -= n;
      return *this;
    }

    iterator operator-(difference_type n) const {
      iterator tmp = *this;
      return tmp -= n;
    }

    difference_type operator-(const iterator &other) const {
      return index - other.index;
    }

    reference operator[](difference_type n) const {
      return (*container)[index + n];
    }

    bool operator==(const iterator &other) const {
      return container == other.container && index == other.index;
    }

    bool operator!=(const iterator &other) const { return !(*this == other); }

    bool operator<(const iterator &other) const { return index < other.index; }

    bool operator>(const iterator &other) const { return other < *this; }

    bool operator<=(const iterator &other) const { return !(other < *this); }

    bool operator>=(const iterator &other) const { return !(*this < other); }
  };

  class const_iterator {
  private:
    const smallvector *container;
    size_type index;

  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using pointer = const T *;
    using reference = const T &;

    const_iterator(const smallvector *container, size_type index)
        : container(container), index(index) {}

    const_iterator(const iterator &it)
        : container(it.container), index(it.index) {}

    reference operator*() const { return (*container)[index]; }

    pointer operator->() const { return &((*container)[index]); }

    const_iterator &operator++() {
      ++index;
      return *this;
    }

    const_iterator operator++(int) {
      const_iterator tmp = *this;
      ++(*this);
      return tmp;
    }

    const_iterator &operator--() {
      --index;
      return *this;
    }

    const_iterator operator--(int) {
      const_iterator tmp = *this;
      --(*this);
      return tmp;
    }

    const_iterator &operator+=(difference_type n) {
      index += n;
      return *this;
    }

    const_iterator operator+(difference_type n) const {
      const_iterator tmp = *this;
      return tmp += n;
    }

    const_iterator &operator-=(difference_type n) {
      index -= n;
      return *this;
    }

    const_iterator operator-(difference_type n) const {
      const_iterator tmp = *this;
      return tmp -= n;
    }

    difference_type operator-(const const_iterator &other) const {
      return index - other.index;
    }

    reference operator[](difference_type n) const {
      return (*container)[index + n];
    }

    bool operator==(const const_iterator &other) const {
      return container == other.container && index == other.index;
    }

    bool operator!=(const const_iterator &other) const {
      return !(*this == other);
    }

    bool operator<(const const_iterator &other) const {
      return index < other.index;
    }

    bool operator>(const const_iterator &other) const { return other < *this; }

    bool operator<=(const const_iterator &other) const {
      return !(other < *this);
    }

    bool operator>=(const const_iterator &other) const {
      return !(*this < other);
    }
  };

  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  // Constructors
  smallvector() noexcept : using_small_buffer(true), curr_size(0) {}

  explicit smallvector(size_type count)
      : using_small_buffer(count <= N), curr_size(count) {
    if (using_small_buffer) {
      for (size_type i = 0; i < count; ++i) {
        new (&small_buffer[i]) T();
      }
    } else {
      large_buffer.resize(count);
    }
  }

  smallvector(size_type count, const T &value)
      : using_small_buffer(count <= N), curr_size(count) {
    if (using_small_buffer) {
      for (size_type i = 0; i < count; ++i) {
        new (&small_buffer[i]) T(value);
      }
    } else {
      large_buffer.resize(count, value);
    }
  }

  template <class InputIt,
            typename = std::enable_if_t<!std::is_integral<InputIt>::value>>
  smallvector(InputIt first, InputIt last) {
    const auto count = std::distance(first, last);
    using_small_buffer = count <= N;
    curr_size = count;

    if (using_small_buffer) {
      size_type i = 0;
      for (auto it = first; it != last; ++it, ++i) {
        new (&small_buffer[i]) T(*it);
      }
    } else {
      large_buffer.assign(first, last);
    }
  }

  smallvector(const smallvector &other)
      : using_small_buffer(other.using_small_buffer),
        curr_size(other.curr_size) {
    if (using_small_buffer) {
      for (size_type i = 0; i < curr_size; ++i) {
        new (&small_buffer[i]) T(other.small_buffer[i]);
      }
    } else {
      large_buffer = other.large_buffer;
    }
  }

  smallvector(smallvector &&other) noexcept
      : using_small_buffer(other.using_small_buffer),
        curr_size(other.curr_size) {
    if (using_small_buffer) {
      for (size_type i = 0; i < curr_size; ++i) {
        new (&small_buffer[i]) T(std::move(other.small_buffer[i]));
        other.small_buffer[i].~T();
      }
    } else {
      large_buffer = std::move(other.large_buffer);
    }
    other.curr_size = 0;
    other.using_small_buffer = true;
  }

  smallvector(std::initializer_list<T> init)
      : using_small_buffer(init.size() <= N), curr_size(init.size()) {
    if (using_small_buffer) {
      size_type i = 0;
      for (const auto &item : init) {
        new (&small_buffer[i++]) T(item);
      }
    } else {
      large_buffer = init;
    }
  }

  // Destructor
  ~smallvector() { clear(); }

  // Assignment operators
  smallvector &operator=(const smallvector &other) {
    if (this != &other) {
      clear();

      curr_size = other.curr_size;
      using_small_buffer = other.using_small_buffer;

      if (using_small_buffer) {
        for (size_type i = 0; i < curr_size; ++i) {
          new (&small_buffer[i]) T(other.small_buffer[i]);
        }
      } else {
        large_buffer = other.large_buffer;
      }
    }
    return *this;
  }

  smallvector &operator=(smallvector &&other) noexcept {
    if (this != &other) {
      clear();

      curr_size = other.curr_size;
      using_small_buffer = other.using_small_buffer;

      if (using_small_buffer) {
        for (size_type i = 0; i < curr_size; ++i) {
          new (&small_buffer[i]) T(std::move(other.small_buffer[i]));
          other.small_buffer[i].~T();
        }
      } else {
        large_buffer = std::move(other.large_buffer);
      }

      other.curr_size = 0;
      other.using_small_buffer = true;
    }
    return *this;
  }

  smallvector &operator=(std::initializer_list<T> ilist) {
    clear();

    curr_size = ilist.size();
    using_small_buffer = ilist.size() <= N;

    if (using_small_buffer) {
      size_type i = 0;
      for (const auto &item : ilist) {
        new (&small_buffer[i++]) T(item);
      }
    } else {
      large_buffer = ilist;
    }

    return *this;
  }

  // Element access
  reference at(size_type pos) {
    if (pos >= curr_size) {
      throw std::out_of_range(
          "smallvector::at: pos (which is " + std::to_string(pos) +
          ") >= this->size() (which is " + std::to_string(curr_size) + ")");
    }
    return operator[](pos);
  }

  const_reference at(size_type pos) const {
    if (pos >= curr_size) {
      throw std::out_of_range(
          "smallvector::at: pos (which is " + std::to_string(pos) +
          ") >= this->size() (which is " + std::to_string(curr_size) + ")");
    }
    return operator[](pos);
  }

  reference operator[](size_type pos) {
    if (using_small_buffer) {
      return small_buffer[pos];
    } else {
      return large_buffer[pos];
    }
  }

  const_reference operator[](size_type pos) const {
    if (using_small_buffer) {
      return small_buffer[pos];
    } else {
      return large_buffer[pos];
    }
  }

  reference front() { return operator[](0); }

  const_reference front() const { return operator[](0); }

  reference back() { return operator[](curr_size - 1); }

  const_reference back() const { return operator[](curr_size - 1); }

  T *data() noexcept {
    if (using_small_buffer) {
      return small_buffer.data();
    } else {
      return large_buffer.data();
    }
  }

  const T *data() const noexcept {
    if (using_small_buffer) {
      return small_buffer.data();
    } else {
      return large_buffer.data();
    }
  }

  // Iterators
  iterator begin() noexcept { return iterator(this, 0); }

  const_iterator begin() const noexcept { return const_iterator(this, 0); }

  const_iterator cbegin() const noexcept { return const_iterator(this, 0); }

  iterator end() noexcept { return iterator(this, curr_size); }

  const_iterator end() const noexcept {
    return const_iterator(this, curr_size);
  }

  const_iterator cend() const noexcept {
    return const_iterator(this, curr_size);
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
  bool empty() const noexcept { return curr_size == 0; }

  size_type size() const noexcept { return curr_size; }

  size_type max_size() const noexcept { return large_buffer.max_size(); }

  void reserve(size_type new_cap) {
    if (new_cap > N && using_small_buffer) {
      switch_to_large_buffer();
    }

    if (!using_small_buffer) {
      large_buffer.reserve(new_cap);
    }
  }

  size_type capacity() const noexcept {
    if (using_small_buffer) {
      return N;
    } else {
      return large_buffer.capacity();
    }
  }

  void shrink_to_fit() {
    if (!using_small_buffer) {
      if (curr_size <= N) {
        // Move back to small buffer
        for (size_type i = 0; i < curr_size; ++i) {
          new (&small_buffer[i]) T(std::move(large_buffer[i]));
        }
        large_buffer.clear();
        using_small_buffer = true;
      } else {
        large_buffer.shrink_to_fit();
      }
    }
  }

  // Modifiers
  void clear() noexcept {
    if (using_small_buffer) {
      for (size_type i = 0; i < curr_size; ++i) {
        small_buffer[i].~T();
      }
    } else {
      large_buffer.clear();
    }
    curr_size = 0;
    using_small_buffer = true;
  }

  iterator insert(const_iterator pos, const T &value) {
    const auto index = pos.index;
    if (curr_size < N && using_small_buffer) {
      // Shift elements to make space
      for (size_type i = curr_size; i > index; --i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i - 1]));
        small_buffer[i - 1].~T();
      }
      // Insert new element
      new (&small_buffer[index]) T(value);
      ++curr_size;
    } else if (curr_size == N && using_small_buffer) {
      // Need to switch to large buffer
      switch_to_large_buffer();
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, value);
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    } else {
      // Already using large buffer
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, value);
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    }
    return iterator(this, index);
  }

  iterator insert(const_iterator pos, T &&value) {
    const auto index = pos.index;
    if (curr_size < N && using_small_buffer) {
      // Shift elements to make space
      for (size_type i = curr_size; i > index; --i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i - 1]));
        small_buffer[i - 1].~T();
      }
      // Insert new element
      new (&small_buffer[index]) T(std::move(value));
      ++curr_size;
    } else if (curr_size == N && using_small_buffer) {
      // Need to switch to large buffer
      switch_to_large_buffer();
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, std::move(value));
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    } else {
      // Already using large buffer
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, std::move(value));
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    }
    return iterator(this, index);
  }

  iterator insert(const_iterator pos, size_type count, const T &value) {
    if (count == 0)
      return iterator(this, pos.index);

    const auto index = pos.index;
    const auto new_size = curr_size + count;

    if (new_size <= N && using_small_buffer) {
      // Shift elements to make space
      for (size_type i = curr_size + count - 1; i >= index + count; --i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i - count]));
        small_buffer[i - count].~T();
      }
      // Insert new elements
      for (size_type i = 0; i < count; ++i) {
        new (&small_buffer[index + i]) T(value);
      }
    } else if (using_small_buffer) {
      // Need to switch to large buffer
      switch_to_large_buffer();
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, count, value);
      curr_size = new_size;
      return iterator(this, it - large_buffer.begin());
    } else {
      // Already using large buffer
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, count, value);
      curr_size = new_size;
      return iterator(this, it - large_buffer.begin());
    }
    curr_size = new_size;
    return iterator(this, index);
  }

  template <class InputIt,
            typename = std::enable_if_t<!std::is_integral<InputIt>::value>>
  iterator insert(const_iterator pos, InputIt first, InputIt last) {
    const auto index = pos.index;
    const auto count = std::distance(first, last);
    if (count == 0)
      return iterator(this, index);

    const auto new_size = curr_size + count;

    if (new_size <= N && using_small_buffer) {
      // Shift elements to make space
      for (size_type i = curr_size + count - 1; i >= index + count; --i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i - count]));
        small_buffer[i - count].~T();
      }
      // Insert new elements
      size_type i = 0;
      for (auto it = first; it != last; ++it, ++i) {
        new (&small_buffer[index + i]) T(*it);
      }
    } else if (using_small_buffer) {
      // Need to switch to large buffer
      switch_to_large_buffer();
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, first, last);
      curr_size = new_size;
      return iterator(this, it - large_buffer.begin());
    } else {
      // Already using large buffer
      auto it = large_buffer.begin() + index;
      it = large_buffer.insert(it, first, last);
      curr_size = new_size;
      return iterator(this, it - large_buffer.begin());
    }
    curr_size = new_size;
    return iterator(this, index);
  }

  iterator insert(const_iterator pos, std::initializer_list<T> ilist) {
    return insert(pos, ilist.begin(), ilist.end());
  }

  template <class... Args>
  iterator emplace(const_iterator pos, Args &&...args) {
    const auto index = pos.index;
    if (curr_size < N && using_small_buffer) {
      // Shift elements to make space
      for (size_type i = curr_size; i > index; --i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i - 1]));
        small_buffer[i - 1].~T();
      }
      // Emplace new element
      new (&small_buffer[index]) T(std::forward<Args>(args)...);
      ++curr_size;
    } else if (curr_size == N && using_small_buffer) {
      // Need to switch to large buffer
      switch_to_large_buffer();
      auto it = large_buffer.begin() + index;
      it = large_buffer.emplace(it, std::forward<Args>(args)...);
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    } else {
      // Already using large buffer
      auto it = large_buffer.begin() + index;
      it = large_buffer.emplace(it, std::forward<Args>(args)...);
      ++curr_size;
      return iterator(this, it - large_buffer.begin());
    }
    return iterator(this, index);
  }

  iterator erase(const_iterator pos) { return erase(pos, pos + 1); }

  iterator erase(const_iterator first, const_iterator last) {
    const auto start_index = first.index;
    const auto end_index = last.index;
    const auto count = end_index - start_index;

    if (count == 0)
      return iterator(this, start_index);

    if (using_small_buffer) {
      // Destroy erased elements
      for (size_type i = start_index; i < end_index; ++i) {
        small_buffer[i].~T();
      }
      // Shift elements to close the gap
      for (size_type i = start_index; i < curr_size - count; ++i) {
        new (&small_buffer[i]) T(std::move(small_buffer[i + count]));
        small_buffer[i + count].~T();
      }
    } else {
      auto it_first = large_buffer.begin() + start_index;
      auto it_last = large_buffer.begin() + end_index;
      large_buffer.erase(it_first, it_last);
    }

    curr_size -= count;

    return iterator(this, start_index);
  }

  void push_back(const T &value) {
    if (curr_size < N && using_small_buffer) {
      new (&small_buffer[curr_size]) T(value);
    } else if (curr_size == N && using_small_buffer) {
      switch_to_large_buffer();
      large_buffer.push_back(value);
    } else {
      large_buffer.push_back(value);
    }
    ++curr_size;
  }

  void push_back(T &&value) {
    if (curr_size < N && using_small_buffer) {
      new (&small_buffer[curr_size]) T(std::move(value));
    } else if (curr_size == N && using_small_buffer) {
      switch_to_large_buffer();
      large_buffer.push_back(std::move(value));
    } else {
      large_buffer.push_back(std::move(value));
    }
    ++curr_size;
  }

  template <class... Args> reference emplace_back(Args &&...args) {
    if (curr_size < N && using_small_buffer) {
      new (&small_buffer[curr_size]) T(std::forward<Args>(args)...);
      ++curr_size;
      return small_buffer[curr_size - 1];
    } else if (curr_size == N && using_small_buffer) {
      switch_to_large_buffer();
      large_buffer.emplace_back(std::forward<Args>(args)...);
      ++curr_size;
      return large_buffer.back();
    } else {
      large_buffer.emplace_back(std::forward<Args>(args)...);
      ++curr_size;
      return large_buffer.back();
    }
  }

  void pop_back() {
    if (curr_size > 0) {
      if (using_small_buffer) {
        small_buffer[curr_size - 1].~T();
      } else {
        large_buffer.pop_back();
      }
      --curr_size;
    }
  }

  void resize(size_type count) {
    if (count < curr_size) {
      if (using_small_buffer) {
        for (size_type i = count; i < curr_size; ++i) {
          small_buffer[i].~T();
        }
      } else {
        large_buffer.resize(count);
      }
    } else if (count > curr_size) {
      if (count <= N && using_small_buffer) {
        for (size_type i = curr_size; i < count; ++i) {
          new (&small_buffer[i]) T();
        }
      } else if (using_small_buffer) {
        switch_to_large_buffer();
        large_buffer.resize(count);
      } else {
        large_buffer.resize(count);
      }
    }
    curr_size = count;
  }

  void resize(size_type count, const value_type &value) {
    if (count < curr_size) {
      if (using_small_buffer) {
        for (size_type i = count; i < curr_size; ++i) {
          small_buffer[i].~T();
        }
      } else {
        large_buffer.resize(count, value);
      }
    } else if (count > curr_size) {
      if (count <= N && using_small_buffer) {
        for (size_type i = curr_size; i < count; ++i) {
          new (&small_buffer[i]) T(value);
        }
      } else if (using_small_buffer) {
        switch_to_large_buffer();
        large_buffer.resize(count, value);
      } else {
        large_buffer.resize(count, value);
      }
    }
    curr_size = count;
  }

  void swap(smallvector &other) noexcept {
    if (using_small_buffer && other.using_small_buffer) {
      // Both using small buffers - swap elements one by one
      const size_type min_size = std::min(curr_size, other.curr_size);

      // Swap common elements
      for (size_type i = 0; i < min_size; ++i) {
        std::swap(small_buffer[i], other.small_buffer[i]);
      }

      // Handle remaining elements if sizes differ
      if (curr_size > other.curr_size) {
        for (size_type i = min_size; i < curr_size; ++i) {
          new (&other.small_buffer[i]) T(std::move(small_buffer[i]));
          small_buffer[i].~T();
        }
      } else if (other.curr_size > curr_size) {
        for (size_type i = min_size; i < other.curr_size; ++i) {
          new (&small_buffer[i]) T(std::move(other.small_buffer[i]));
          other.small_buffer[i].~T();
        }
      }
    } else if (!using_small_buffer && !other.using_small_buffer) {
      // Both using large buffers - swap vectors
      large_buffer.swap(other.large_buffer);
    } else if (using_small_buffer && !other.using_small_buffer) {
      // this is small, other is large
      std::vector<T> temp = std::move(other.large_buffer);

      // Move this's elements to other's small buffer
      for (size_type i = 0; i < curr_size; ++i) {
        new (&other.small_buffer[i]) T(std::move(small_buffer[i]));
        small_buffer[i].~T();
      }

      // Take ownership of other's large buffer
      large_buffer = std::move(temp);
    } else { // !using_small_buffer && other.using_small_buffer
      // this is large, other is small
      std::vector<T> temp = std::move(large_buffer);

      // Move other's elements to this's small buffer
      for (size_type i = 0; i < other.curr_size; ++i) {
        new (&small_buffer[i]) T(std::move(other.small_buffer[i]));
        other.small_buffer[i].~T();
      }

      // Give other ownership of this's large buffer
      other.large_buffer = std::move(temp);
    }

    // Swap metadata
    std::swap(curr_size, other.curr_size);
    std::swap(using_small_buffer, other.using_small_buffer);
  }
};

// Non-member functions
template <typename T, std::size_t N>
void swap(smallvector<T, N> &lhs, smallvector<T, N> &rhs) noexcept {
  lhs.swap(rhs);
}

template <typename T, std::size_t N>
bool operator==(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  if (lhs.size() != rhs.size()) {
    return false;
  }

  for (std::size_t i = 0; i < lhs.size(); ++i) {
    if (!(lhs[i] == rhs[i])) {
      return false;
    }
  }

  return true;
}

template <typename T, std::size_t N>
bool operator!=(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  return !(lhs == rhs);
}

template <typename T, std::size_t N>
bool operator<(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(),
                                      rhs.end());
}

template <typename T, std::size_t N>
bool operator<=(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  return !(rhs < lhs);
}

template <typename T, std::size_t N>
bool operator>(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  return rhs < lhs;
}

template <typename T, std::size_t N>
bool operator>=(const smallvector<T, N> &lhs, const smallvector<T, N> &rhs) {
  return !(lhs < rhs);
}

#endif // smallvector_HPP