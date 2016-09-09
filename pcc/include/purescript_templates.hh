///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript_memory.hh
// Copyright   :  (c) Andy Arvanitis 2016
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Generic helper templates
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PureScript_TEMPLATES_HH
#define PureScript_TEMPLATES_HH

#define ALL_VERSIONS(M) \
  M(0) \
  M(1) \
  M(2) \
  M(3) \
  M(4) \
  M(5) \
  M(6) \
  M(7) \
  M(8) \
  M(9) \
  M(10) \
  M(11) \
  M(12)

#define PARAMS_FOR_ARITY_0(P)
#define PARAMS_FOR_ARITY_1(P)  const P&
#define PARAMS_FOR_ARITY_2(P)  const P&, const P&
#define PARAMS_FOR_ARITY_3(P)  const P&, const P&, const P&
#define PARAMS_FOR_ARITY_4(P)  const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_5(P)  const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_6(P)  const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_7(P)  const P&, const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_8(P)  const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_9(P)  const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_10(P) const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_11(P) const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&
#define PARAMS_FOR_ARITY_12(P) const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&

namespace PureScript {

  template <typename T, typename... Args>
  using std_function = std::function<T(Args...)>;

  template <size_t N, typename P>
  struct for_parameters {
  };

  template <typename P>
  struct for_parameters<0, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T>;
  };

  template <typename P>
  struct for_parameters<1, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&>;
  };

  template <typename P>
  struct for_parameters<2, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<3, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<4, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<5, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<6, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<7, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<8, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<9, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<10, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<11, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

  template <typename P>
  struct for_parameters<12, P> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&, const P&>;
  };

}

#endif // PureScript_TEMPLATES_HH
