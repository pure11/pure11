///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Basic types and functions to support purescript-to-C++1x rendering
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PureScript_HH
#define PureScript_HH

#if !defined(__cplusplus)
#error This is a C++ file!
#endif

// Standard includes

#if defined(DEBUG)
  #include <cassert>
  #include <limits>
  #define IF_DEBUG(x) x
#else
  #define NDEBUG
  #undef assert
  #define assert(x)
  #define IF_DEBUG(_)
#endif

#include <functional>
#include <string>
#include <array>
#include <deque>
#include <map>
#include <utility>
#include <stdexcept>
#include <iso646.h> // mostly for MS Visual Studio compiler
#include "purescript_memory.hh"

namespace PureScript {

using cstring = const char *;
using std::string;
using std::nullptr_t;
using std::runtime_error;

namespace Private {
  struct SymbolGeneratorAnchor {};

  template <typename T>
  struct SymbolGenerator {
    constexpr static SymbolGeneratorAnchor anchor = SymbolGeneratorAnchor{};
  };
  template <typename T>
  constexpr SymbolGeneratorAnchor SymbolGenerator<T>::anchor;

  using Symbol = const SymbolGeneratorAnchor *;
}

#define DEFINE_SYMBOL(S) namespace PureScript { \
                           namespace Private { \
                             namespace Symbols { \
                               struct S_ ## S {}; \
                             } \
                           } \
                         }
#define SYM(S) (&Private::SymbolGenerator<::PureScript::Private::Symbols::S_ ## S>::anchor)

constexpr bool undefined = false;

// TODO: Not a real limit, just used for simpler accessors
static constexpr size_t unknown_size = 64;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  private:
  static constexpr auto _managed_ = 0x10;

  public:
  enum class Tag {
    Unknown,
    Thunk,
    Integer,
    Double,
    Character,
    Boolean,
    Function,
    EffFunction,
    RawPointer,
    String = _managed_,
    Dictionary,
    Data,
    Array,
    Record,
    Closure,
    EffClosure,
    Pointer,
    Invalid
  };

  private:
  mutable Tag tag;

  inline static auto isManaged(const Tag tag) -> decltype(_managed_) {
    return static_cast<decltype(_managed_)>(tag) & _managed_;
  }
  static_assert(static_cast<decltype(_managed_)>(Tag::Invalid) & _managed_, "outside of range");

  public:
  struct as_thunk {
  };
  static constexpr as_thunk unthunk = as_thunk{};

  using dict_pair = std::pair<const Private::Symbol, const any>;
  template <size_t N>
  using dict = std::array<const dict_pair, N>;

  template <size_t N>
  using data = std::array<const any, N>;

  using array = std::deque<any WITH_ALLOCATOR(any)>;

  struct cstr_cmp {
    auto operator ()(const char *, const char *) const -> bool;
  };
  using record = std::map<const char *,
                          any,
                          cstr_cmp
                          WITH_ALLOCATOR_PAIR(char * const, any)>;

  using fn     = auto (*)(const any&) -> any;
  using eff_fn = auto (*)() -> any;
  using thunk  = auto (*)(const as_thunk) -> const any&;

  private:
  class Closure {
    public:
      virtual auto operator()(const any&) const -> any = 0;
      virtual ~Closure();
  };

  template <typename T>
  class Closure_ : public Closure {
    const T lambda;
  public:
    Closure_(const T& l) noexcept : lambda(l) {}
    auto operator()(const any& arg) const -> any override {
      return lambda(arg);
    }
  };

  class EffClosure {
    public:
      virtual auto operator()() const -> any = 0;
      virtual ~EffClosure();
  };

  template <typename T>
  class EffClosure_ : public EffClosure {
    const T lambda;
  public:
    EffClosure_(const T& l) noexcept : lambda(l) {}
    auto operator()() const -> any override {
      return lambda();
    }
  };

  private:
  union {
    mutable thunk         t;
    mutable int           i;
    mutable double        d;
    mutable char32_t      c;
    mutable bool          b;
    mutable fn            f;
    mutable eff_fn        e;
    mutable void *        v;
    mutable uint64_t      u;
    mutable managed<void> p;
  };

  public:

  any(const int val) noexcept : tag(Tag::Integer), i(val) {}
  any(const long val) noexcept : tag(Tag::Integer), i(static_cast<decltype(i)>(val)) {
    assert(val >= std::numeric_limits<decltype(i)>::min() &&
           val <= std::numeric_limits<decltype(i)>::max());
  }
  any(const double val) noexcept : tag(Tag::Double), d(val) {}
  any(const char32_t val) noexcept : tag(Tag::Character), c(val) {}
  any(const char val) noexcept : tag(Tag::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) noexcept : tag(Tag::Boolean), b(val) {}

  any(const char * val) : tag(Tag::String), p(make_managed_and_finalized<string>(val)) {}
  any(const string& val) : tag(Tag::String), p(make_managed_and_finalized<string>(val)) {}
  any(string&& val) noexcept : tag(Tag::String), p(make_managed_and_finalized<string>(std::move(val))) {}

  any(const managed<string>& val) noexcept : tag(Tag::String), p(val) {}
  any(managed<string>&& val) noexcept : tag(Tag::String), p(std::move(val)) {}

  template <size_t N>
  any(dict<N>&& val) noexcept : tag(Tag::Dictionary), p(make_managed<dict<N>>(std::move(val))) {}

  template <size_t N>
  any(data<N>&& val) noexcept : tag(Tag::Data), p(make_managed<data<N>>(std::move(val))) {}

  any(const array& val) : tag(Tag::Array), p(make_managed<array>(val)) {}
  any(array&& val) noexcept : tag(Tag::Array), p(make_managed<array>(std::move(val))) {}

  any(const record& val) : tag(Tag::Record), p(make_managed<record>(val)) {}
  any(record&& val) noexcept : tag(Tag::Record), p(make_managed<record>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0) noexcept
    : tag(Tag::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any(const any&)>,T>::value>::type* = 0)
    : tag(Tag::Closure), p(make_managed<Closure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,eff_fn>::value>::type* = 0) noexcept
    : tag(Tag::EffFunction), e(val) {}

  template <typename T,
            typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                               !std::is_convertible<T,eff_fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any()>,T>::value>::type* = 0)
    : tag(Tag::EffClosure), p(make_managed<EffClosure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,thunk>::value>::type* = 0) noexcept
    : tag(Tag::Thunk), t(val) {}

  template <typename T,
            typename = typename std::enable_if<std::is_class<typename std::remove_pointer<T>::type>::value>::type>
  any(const T& val, typename std::enable_if<IS_POINTER_TYPE(T)::value>::type* = 0) noexcept
    : tag(Tag::Pointer), p(val) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<managed<void>,T>::value>::type* = 0) noexcept
    : tag(Tag::Pointer), p(std::move(val)) {}

  any(void * val) noexcept : tag(Tag::RawPointer), v(val) {}
  any(nullptr_t) noexcept : tag(Tag::RawPointer), v(nullptr) {}

#if !defined(USE_GC)
  private:
  auto destruct() const noexcept -> void {
    if (isManaged(tag)) {
      p.~managed<void>();
    }
  }

  static_assert(sizeof(any::u) >= sizeof(any::t), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::i), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::d), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::c), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::b), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::f), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::e), "size assumption");
  static_assert(sizeof(any::u) >= sizeof(any::v), "size assumption");

  auto copyAssign(const any& other) const noexcept -> void {
    if (isManaged(other.tag)) {
      new (&p) managed<void>(other.p);
    } else {
      u = other.u;
    }
  }
  auto moveAssign(any& other) const noexcept -> void {
    if (isManaged(other.tag)) {
      new (&p) managed<void>(std::move(other.p));
    } else {
      u = other.u;
    }
  }

  public:
  any(const any& other) noexcept : tag(other.tag) {
    copyAssign(other);
  }

  any(any&& other) noexcept : tag(other.tag) {
    moveAssign(other);
  }

  auto operator=(const any& rhs) noexcept -> any& {
    destruct();
    tag = rhs.tag;
    copyAssign(rhs);
    return *this;
  }

  auto operator=(any&& rhs) noexcept -> any& {
    destruct();
    tag = rhs.tag;
    moveAssign(rhs);
    return *this;
  }

  ~any() {
    destruct();
  }
#endif // !defined(USE_GC)

  public:
  any() = delete;

  auto operator()(const any&) const -> any;
  auto operator()(const as_thunk) const -> const any&;
  auto operator()() const -> any;

  operator int() const;
  operator double() const;
  operator bool() const;
  operator char32_t() const;
  operator char() const;
  operator size_t() const;
  operator const string&() const;
  operator const char *() const;
  operator const array&() const;
  operator const record&() const;

  operator void *() const {
    return extractPointer(IF_DEBUG(Tag::RawPointer));
  }

  template <typename T,
            typename = typename std::enable_if<std::is_pointer<T>::value &&
                                               std::is_class<typename std::remove_pointer<T>::type>::value>::type>
  operator T() const {
    return static_cast<T>(extractPointer(IF_DEBUG(Tag::RawPointer)));
  }

  auto operator[](const size_t) const -> const any&;
  auto at(const char *) const -> const any&;
  auto size() const -> size_t;
  auto empty() const -> bool;
  auto contains(const Private::Symbol) const -> bool;

  auto extractPointer(IF_DEBUG(const Tag)) const -> void*;

  auto rawPointer() const -> void* {
    const any& variant = unthunkVariant(*this);
    assert(tag == Tag::RawPointer);
    return variant.v;
  }

  static auto unthunkVariant(const any&) -> const any&;

  #define DEFINE_OPERATOR(op) \
    template <typename T, \
              typename = typename std::enable_if<!std::is_same<T,any>::value && \
                                                 !std::is_same<T,const char *>::value>::type> \
    inline friend auto operator op (const any& lhs, T rhs) -> T { \
      return static_cast<typename std::add_lvalue_reference<typename std::add_const<T>::type >::type>(lhs) op rhs; \
    } \
    template <typename T, \
              typename = typename std::enable_if<!std::is_same<T,any>::value && \
                                                 !std::is_same<T,const char *>::value>::type> \
    inline friend auto operator op (T lhs, const any& rhs) -> T { \
      return lhs op static_cast<typename std::add_lvalue_reference<typename std::add_const<T>::type >::type>(rhs); \
    } \
    friend auto operator op (const any&, const any&) -> any; \

  #define DEFINE_COMPARISON_OPERATOR(op) \
    template <typename T, \
              typename = typename std::enable_if<!std::is_same<T,any>::value && \
                                                 !std::is_same<T,const char *>::value>::type> \
    inline friend auto operator op (const any& lhs, T rhs) -> bool { \
      return static_cast<typename std::add_lvalue_reference<typename std::add_const<T>::type >::type>(lhs) op rhs; \
    } \
    template <typename T, \
              typename = typename std::enable_if<!std::is_same<T,any>::value && \
                                                 !std::is_same<T,const char *>::value>::type> \
    inline friend auto operator op (T lhs, const any& rhs) -> bool { \
      return lhs op static_cast<typename std::add_lvalue_reference<typename std::add_const<T>::type >::type>(rhs); \
    } \
    inline friend auto operator op (const any& lhs, const char * rhs) -> bool { \
      return static_cast<const string&>(lhs) op rhs; \
    } \
    inline friend auto operator op (const char * lhs, const any& rhs) -> bool { \
      return lhs op static_cast<const string&>(rhs); \
    } \
    friend auto operator op (const any&, const any&) -> bool; \

  DEFINE_COMPARISON_OPERATOR(==)
  DEFINE_COMPARISON_OPERATOR(!=)
  DEFINE_COMPARISON_OPERATOR(<)
  DEFINE_COMPARISON_OPERATOR(<=)
  DEFINE_COMPARISON_OPERATOR(>)
  DEFINE_COMPARISON_OPERATOR(>=)

  DEFINE_OPERATOR(+)
  DEFINE_OPERATOR(-)
  DEFINE_OPERATOR(*)
  DEFINE_OPERATOR(/)
  DEFINE_OPERATOR(%)

  inline friend auto operator+(const any& lhs, const char * rhs) -> string {
    return static_cast<const string&>(lhs) + rhs;
  }
  inline friend auto operator+(const char * lhs, const any& rhs) -> string {
    return lhs + static_cast<const string&>(rhs);
  }

  friend auto operator-(const any&) -> any; // unary negate

}; // class any

namespace Private {
  template <typename T, typename U=void>
  struct TagHelper {};

  template <>
  struct TagHelper<string> {
    static constexpr any::Tag tag = any::Tag::String;
  };

  template <>
  struct TagHelper<const char *> {
    static constexpr any::Tag tag = any::Tag::String;
  };

  template <size_t N>
  struct TagHelper<any::dict<N>> {
    static constexpr any::Tag tag = any::Tag::Dictionary;
  };

  template <size_t N>
  struct TagHelper<any::data<N>> {
    static constexpr any::Tag tag = any::Tag::Data;
  };

  template <>
  struct TagHelper<any::array> {
    static constexpr any::Tag tag = any::Tag::Array;
  };

  template <>
  struct TagHelper<any::record> {
    static constexpr any::Tag tag = any::Tag::Record;
  };

  template <typename T>
  struct TagHelper<T> {
    static constexpr any::Tag tag = any::Tag::Pointer;
  };
}

// Pass-through
template <typename T>
constexpr auto cast(const T& a) -> const T& {
  return a;
}

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_arithmetic<T>::value ||
                            std::is_same<T,const char *>::value, T>::type {
  return a;
}

template <typename T,
          typename = typename std::enable_if<std::is_class<T>::value>::type>
inline auto cast(const any& a) -> T& {
  return *static_cast<T*>(a.extractPointer(IF_DEBUG(Private::TagHelper<T>::tag)));
}

template <typename T,
          typename = typename std::enable_if<std::is_pointer<T>::value &&
                                            !std::is_same<T,void*>::value &&
                                            !std::is_same<T,const char *>::value>::type>
inline auto cast(const any& a) -> T {
  return static_cast<T>(a.extractPointer(IF_DEBUG(Private::TagHelper<T>::tag)));
}

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_same<void*, T>::value, T>::type {
  return a.rawPointer();
}

namespace dict {
  template <size_t N, typename T>
  inline auto get(const T& a) ->
      typename std::enable_if<!std::is_same<any, T>::value, const any&>::type {
    return a[N].second;
  }

  template <size_t N>
  inline auto get(const any& a) -> const any& {
    return cast<any::dict<N+1>>(a)[N].second;
  }

  template <size_t N>
  inline auto get(const Private::Symbol key, const any::dict<N>& a) -> const any& {
    static_assert(N > 0, "dict size must be greater than zero");
    typename std::remove_reference<decltype(a)>::type::size_type i = 0;
    do {
      if (a[i].first == key) {
        return a[i].second;
      }
    } while (a[++i].first != nullptr);
    assert(false && "dict key not found");
    static const any invalid_key(nullptr);
    return invalid_key;
  }

  inline auto get(const Private::Symbol key, const any& a) -> const any& {
    return get(key, cast<any::dict<unknown_size>>(a));
  }

  #define P(N)    any::dict_pair&& arg ## N
  #define T(N)    any::dict<N+1>
  #define E(N)    std::forward<decltype(arg ## N)>(arg ## N)
  #define M(...)  {{ __VA_ARGS__, { nullptr, nullptr } }}

  inline auto make(P(1)) -> T(1) {
    return T(1) M(E(1));
  }
  inline auto make(P(1), P(2)) -> T(2) {
    return T(2) M(E(1), E(2));
  }
  inline auto make(P(1), P(2), P(3)) -> T(3) {
    return T(3) M(E(1), E(2), E(3));
  }
  inline auto make(P(1), P(2), P(3), P(4)) -> T(4) {
    return T(4) M(E(1), E(2), E(3), E(4));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5)) -> T(5) {
    return T(5) M(E(1), E(2), E(3), E(4), E(5));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6)) -> T(6) {
    return T(6) M(E(1), E(2), E(3), E(4), E(5), E(6));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6), P(7)) -> T(7) {
    return T(7) M(E(1), E(2), E(3), E(4), E(5), E(6), E(7));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6), P(7), P(8)) -> T(8) {
    return T(8) M(E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8));
  }

  #undef P
  #undef T
  #undef E
  #undef M
}

namespace data {
  template <size_t N, typename T>
  inline auto get(const T& a) ->
      typename std::enable_if<!std::is_same<any, T>::value, const any&>::type {
    return a[N];
  }

  template <size_t N>
  inline auto get(const any& a) -> const any& {
    return cast<any::data<N+1>>(a)[N];
  }

  template <typename T>
  inline auto ctor(const T& a) -> int {
    return get<0>(a);
  }
}

} // namespace PureScript

#undef WITH_ALLOCATOR
#undef WITH_ALLOCATOR_PAIR
#undef IS_POINTER_TYPE
#undef DEFINE_OPERATOR
#undef DEFINE_COMPARISON_OPERATOR

#endif // PureScript_HH
