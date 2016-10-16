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
  M(10)

#define REPLICATE(N, P) _PS_REPLICATE_ ## N (P)

#define _PS_REPLICATE_0(P)
#define _PS_REPLICATE_1(P)  P
#define _PS_REPLICATE_2(P)  P, P
#define _PS_REPLICATE_3(P)  P, P, P
#define _PS_REPLICATE_4(P)  P, P, P, P
#define _PS_REPLICATE_5(P)  P, P, P, P, P
#define _PS_REPLICATE_6(P)  P, P, P, P, P, P
#define _PS_REPLICATE_7(P)  P, P, P, P, P, P, P
#define _PS_REPLICATE_8(P)  P, P, P, P, P, P, P, P
#define _PS_REPLICATE_9(P)  P, P, P, P, P, P, P, P, P
#define _PS_REPLICATE_10(P) P, P, P, P, P, P, P, P, P, P

namespace PureScript {

  template <typename T, typename... Args>
  using std_function = std::function<T(Args...)>;

  template <size_t N, typename P, typename... Args>
  struct for_parameters;

  template <typename P, typename... Args>
  struct for_parameters<0, P, Args...> {
    template <typename T, template <typename, typename...> class U>
    using make_type = U<T, Args...>;
  };

  template <size_t N, typename P, typename... Args>
  struct for_parameters {
    template <typename T, template <typename, typename...> class U>
    using make_type = typename for_parameters<N-1, P, const P&, Args...>::template make_type<T,U>;
  };

}

#endif // PureScript_TEMPLATES_HH
