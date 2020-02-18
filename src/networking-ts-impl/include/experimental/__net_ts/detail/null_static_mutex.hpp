//
// detail/null_static_mutex.hpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2019 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef NET_TS_DETAIL_NULL_STATIC_MUTEX_HPP
#define NET_TS_DETAIL_NULL_STATIC_MUTEX_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
# pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include <experimental/__net_ts/detail/config.hpp>

#if !defined(NET_TS_HAS_THREADS)

#include <experimental/__net_ts/detail/scoped_lock.hpp>

#include <experimental/__net_ts/detail/push_options.hpp>

namespace std {
namespace experimental {
namespace net {
inline namespace v1 {
namespace detail {

struct null_static_mutex
{
  typedef std::experimental::net::v1::detail::scoped_lock<null_static_mutex> scoped_lock;

  // Initialise the mutex.
  void init()
  {
  }

  // Lock the mutex.
  void lock()
  {
  }

  // Unlock the mutex.
  void unlock()
  {
  }

  int unused_;
};

#define NET_TS_NULL_STATIC_MUTEX_INIT { 0 }

} // namespace detail
} // inline namespace v1
} // namespace net
} // namespace experimental
} // namespace std

#include <experimental/__net_ts/detail/pop_options.hpp>

#endif // !defined(NET_TS_HAS_THREADS)

#endif // NET_TS_DETAIL_NULL_STATIC_MUTEX_HPP
