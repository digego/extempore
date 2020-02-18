//
// impl/read.hpp
// ~~~~~~~~~~~~~
//
// Copyright (c) 2003-2019 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef NET_TS_IMPL_READ_HPP
#define NET_TS_IMPL_READ_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
# pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include <algorithm>
#include <experimental/__net_ts/associated_allocator.hpp>
#include <experimental/__net_ts/associated_executor.hpp>
#include <experimental/__net_ts/buffer.hpp>
#include <experimental/__net_ts/completion_condition.hpp>
#include <experimental/__net_ts/detail/array_fwd.hpp>
#include <experimental/__net_ts/detail/base_from_completion_cond.hpp>
#include <experimental/__net_ts/detail/bind_handler.hpp>
#include <experimental/__net_ts/detail/consuming_buffers.hpp>
#include <experimental/__net_ts/detail/dependent_type.hpp>
#include <experimental/__net_ts/detail/handler_alloc_helpers.hpp>
#include <experimental/__net_ts/detail/handler_cont_helpers.hpp>
#include <experimental/__net_ts/detail/handler_invoke_helpers.hpp>
#include <experimental/__net_ts/detail/handler_type_requirements.hpp>
#include <experimental/__net_ts/detail/throw_error.hpp>
#include <experimental/__net_ts/error.hpp>

#include <experimental/__net_ts/detail/push_options.hpp>

namespace std {
namespace experimental {
namespace net {
inline namespace v1 {

namespace detail
{
  template <typename SyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition>
  std::size_t read_buffer_sequence(SyncReadStream& s,
      const MutableBufferSequence& buffers, const MutableBufferIterator&,
      CompletionCondition completion_condition, std::error_code& ec)
  {
    ec = std::error_code();
    std::experimental::net::v1::detail::consuming_buffers<mutable_buffer,
        MutableBufferSequence, MutableBufferIterator> tmp(buffers);
    while (!tmp.empty())
    {
      if (std::size_t max_size = detail::adapt_completion_condition_result(
            completion_condition(ec, tmp.total_consumed())))
        tmp.consume(s.read_some(tmp.prepare(max_size), ec));
      else
        break;
    }
    return tmp.total_consumed();;
  }
} // namespace detail

template <typename SyncReadStream, typename MutableBufferSequence,
    typename CompletionCondition>
std::size_t read(SyncReadStream& s, const MutableBufferSequence& buffers,
    CompletionCondition completion_condition, std::error_code& ec,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  return detail::read_buffer_sequence(s, buffers,
      std::experimental::net::v1::buffer_sequence_begin(buffers), completion_condition, ec);
}

template <typename SyncReadStream, typename MutableBufferSequence>
inline std::size_t read(SyncReadStream& s, const MutableBufferSequence& buffers,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  std::error_code ec;
  std::size_t bytes_transferred = read(s, buffers, transfer_all(), ec);
  std::experimental::net::v1::detail::throw_error(ec, "read");
  return bytes_transferred;
}

template <typename SyncReadStream, typename MutableBufferSequence>
inline std::size_t read(SyncReadStream& s, const MutableBufferSequence& buffers,
    std::error_code& ec,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  return read(s, buffers, transfer_all(), ec);
}

template <typename SyncReadStream, typename MutableBufferSequence,
    typename CompletionCondition>
inline std::size_t read(SyncReadStream& s, const MutableBufferSequence& buffers,
    CompletionCondition completion_condition,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  std::error_code ec;
  std::size_t bytes_transferred = read(s, buffers, completion_condition, ec);
  std::experimental::net::v1::detail::throw_error(ec, "read");
  return bytes_transferred;
}

template <typename SyncReadStream, typename DynamicBuffer,
    typename CompletionCondition>
std::size_t read(SyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    CompletionCondition completion_condition, std::error_code& ec,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  typename decay<DynamicBuffer>::type b(
      NET_TS_MOVE_CAST(DynamicBuffer)(buffers));

  ec = std::error_code();
  std::size_t total_transferred = 0;
  std::size_t max_size = detail::adapt_completion_condition_result(
        completion_condition(ec, total_transferred));
  std::size_t bytes_available = std::min<std::size_t>(
        std::max<std::size_t>(512, b.capacity() - b.size()),
        std::min<std::size_t>(max_size, b.max_size() - b.size()));
  while (bytes_available > 0)
  {
    std::size_t bytes_transferred = s.read_some(b.prepare(bytes_available), ec);
    b.commit(bytes_transferred);
    total_transferred += bytes_transferred;
    max_size = detail::adapt_completion_condition_result(
          completion_condition(ec, total_transferred));
    bytes_available = std::min<std::size_t>(
          std::max<std::size_t>(512, b.capacity() - b.size()),
          std::min<std::size_t>(max_size, b.max_size() - b.size()));
  }
  return total_transferred;
}

template <typename SyncReadStream, typename DynamicBuffer>
inline std::size_t read(SyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  std::error_code ec;
  std::size_t bytes_transferred = read(s,
      NET_TS_MOVE_CAST(DynamicBuffer)(buffers), transfer_all(), ec);
  std::experimental::net::v1::detail::throw_error(ec, "read");
  return bytes_transferred;
}

template <typename SyncReadStream, typename DynamicBuffer>
inline std::size_t read(SyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    std::error_code& ec,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  return read(s, NET_TS_MOVE_CAST(DynamicBuffer)(buffers),
      transfer_all(), ec);
}

template <typename SyncReadStream, typename DynamicBuffer,
    typename CompletionCondition>
inline std::size_t read(SyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    CompletionCondition completion_condition,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  std::error_code ec;
  std::size_t bytes_transferred = read(s,
      NET_TS_MOVE_CAST(DynamicBuffer)(buffers),
      completion_condition, ec);
  std::experimental::net::v1::detail::throw_error(ec, "read");
  return bytes_transferred;
}

namespace detail
{
  template <typename AsyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition,
      typename ReadHandler>
  class read_op
    : detail::base_from_completion_cond<CompletionCondition>
  {
  public:
    read_op(AsyncReadStream& stream, const MutableBufferSequence& buffers,
        CompletionCondition completion_condition, ReadHandler& handler)
      : detail::base_from_completion_cond<
          CompletionCondition>(completion_condition),
        stream_(stream),
        buffers_(buffers),
        start_(0),
        handler_(NET_TS_MOVE_CAST(ReadHandler)(handler))
    {
    }

#if defined(NET_TS_HAS_MOVE)
    read_op(const read_op& other)
      : detail::base_from_completion_cond<CompletionCondition>(other),
        stream_(other.stream_),
        buffers_(other.buffers_),
        start_(other.start_),
        handler_(other.handler_)
    {
    }

    read_op(read_op&& other)
      : detail::base_from_completion_cond<CompletionCondition>(other),
        stream_(other.stream_),
        buffers_(other.buffers_),
        start_(other.start_),
        handler_(NET_TS_MOVE_CAST(ReadHandler)(other.handler_))
    {
    }
#endif // defined(NET_TS_HAS_MOVE)

    void operator()(const std::error_code& ec,
        std::size_t bytes_transferred, int start = 0)
    {
      std::size_t max_size;
      switch (start_ = start)
      {
        case 1:
        max_size = this->check_for_completion(ec, buffers_.total_consumed());
        do
        {
          stream_.async_read_some(buffers_.prepare(max_size),
              NET_TS_MOVE_CAST(read_op)(*this));
          return; default:
          buffers_.consume(bytes_transferred);
          if ((!ec && bytes_transferred == 0) || buffers_.empty())
            break;
          max_size = this->check_for_completion(ec, buffers_.total_consumed());
        } while (max_size > 0);

        handler_(ec, buffers_.total_consumed());
      }
    }

  //private:
    AsyncReadStream& stream_;
    std::experimental::net::v1::detail::consuming_buffers<mutable_buffer,
        MutableBufferSequence, MutableBufferIterator> buffers_;
    int start_;
    ReadHandler handler_;
  };

  template <typename AsyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition,
      typename ReadHandler>
  inline void* networking_ts_handler_allocate(std::size_t size,
      read_op<AsyncReadStream, MutableBufferSequence, MutableBufferIterator,
        CompletionCondition, ReadHandler>* this_handler)
  {
    return networking_ts_handler_alloc_helpers::allocate(
        size, this_handler->handler_);
  }

  template <typename AsyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition,
      typename ReadHandler>
  inline void networking_ts_handler_deallocate(void* pointer, std::size_t size,
      read_op<AsyncReadStream, MutableBufferSequence, MutableBufferIterator,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_alloc_helpers::deallocate(
        pointer, size, this_handler->handler_);
  }

  template <typename AsyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition,
      typename ReadHandler>
  inline bool networking_ts_handler_is_continuation(
      read_op<AsyncReadStream, MutableBufferSequence, MutableBufferIterator,
        CompletionCondition, ReadHandler>* this_handler)
  {
    return this_handler->start_ == 0 ? true
      : networking_ts_handler_cont_helpers::is_continuation(
          this_handler->handler_);
  }

  template <typename Function, typename AsyncReadStream,
      typename MutableBufferSequence, typename MutableBufferIterator,
      typename CompletionCondition, typename ReadHandler>
  inline void networking_ts_handler_invoke(Function& function,
      read_op<AsyncReadStream, MutableBufferSequence, MutableBufferIterator,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_invoke_helpers::invoke(
        function, this_handler->handler_);
  }

  template <typename Function, typename AsyncReadStream,
      typename MutableBufferSequence, typename MutableBufferIterator,
      typename CompletionCondition, typename ReadHandler>
  inline void networking_ts_handler_invoke(const Function& function,
      read_op<AsyncReadStream, MutableBufferSequence, MutableBufferIterator,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_invoke_helpers::invoke(
        function, this_handler->handler_);
  }

  template <typename AsyncReadStream, typename MutableBufferSequence,
      typename MutableBufferIterator, typename CompletionCondition,
      typename ReadHandler>
  inline void start_read_buffer_sequence_op(AsyncReadStream& stream,
      const MutableBufferSequence& buffers, const MutableBufferIterator&,
      CompletionCondition completion_condition, ReadHandler& handler)
  {
    detail::read_op<AsyncReadStream, MutableBufferSequence,
      MutableBufferIterator, CompletionCondition, ReadHandler>(
        stream, buffers, completion_condition, handler)(
          std::error_code(), 0, 1);
  }
} // namespace detail

#if !defined(GENERATING_DOCUMENTATION)

template <typename AsyncReadStream, typename MutableBufferSequence,
    typename MutableBufferIterator, typename CompletionCondition,
    typename ReadHandler, typename Allocator>
struct associated_allocator<
    detail::read_op<AsyncReadStream, MutableBufferSequence,
      MutableBufferIterator, CompletionCondition, ReadHandler>,
    Allocator>
{
  typedef typename associated_allocator<ReadHandler, Allocator>::type type;

  static type get(
      const detail::read_op<AsyncReadStream, MutableBufferSequence,
        MutableBufferIterator, CompletionCondition, ReadHandler>& h,
      const Allocator& a = Allocator()) NET_TS_NOEXCEPT
  {
    return associated_allocator<ReadHandler, Allocator>::get(h.handler_, a);
  }
};

template <typename AsyncReadStream, typename MutableBufferSequence,
    typename MutableBufferIterator, typename CompletionCondition,
    typename ReadHandler, typename Executor>
struct associated_executor<
    detail::read_op<AsyncReadStream, MutableBufferSequence,
      MutableBufferIterator, CompletionCondition, ReadHandler>,
    Executor>
{
  typedef typename associated_executor<ReadHandler, Executor>::type type;

  static type get(
      const detail::read_op<AsyncReadStream, MutableBufferSequence,
        MutableBufferIterator, CompletionCondition, ReadHandler>& h,
      const Executor& ex = Executor()) NET_TS_NOEXCEPT
  {
    return associated_executor<ReadHandler, Executor>::get(h.handler_, ex);
  }
};

#endif // !defined(GENERATING_DOCUMENTATION)

template <typename AsyncReadStream, typename MutableBufferSequence,
    typename CompletionCondition, typename ReadHandler>
inline NET_TS_INITFN_RESULT_TYPE(ReadHandler,
    void (std::error_code, std::size_t))
async_read(AsyncReadStream& s, const MutableBufferSequence& buffers,
    CompletionCondition completion_condition,
    NET_TS_MOVE_ARG(ReadHandler) handler,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  // If you get an error on the following line it means that your handler does
  // not meet the documented type requirements for a ReadHandler.
  NET_TS_READ_HANDLER_CHECK(ReadHandler, handler) type_check;

  async_completion<ReadHandler,
    void (std::error_code, std::size_t)> init(handler);

  detail::start_read_buffer_sequence_op(s, buffers,
      std::experimental::net::v1::buffer_sequence_begin(buffers), completion_condition,
      init.completion_handler);

  return init.result.get();
}

template <typename AsyncReadStream, typename MutableBufferSequence,
    typename ReadHandler>
inline NET_TS_INITFN_RESULT_TYPE(ReadHandler,
    void (std::error_code, std::size_t))
async_read(AsyncReadStream& s, const MutableBufferSequence& buffers,
    NET_TS_MOVE_ARG(ReadHandler) handler,
    typename enable_if<
      is_mutable_buffer_sequence<MutableBufferSequence>::value
    >::type*)
{
  // If you get an error on the following line it means that your handler does
  // not meet the documented type requirements for a ReadHandler.
  NET_TS_READ_HANDLER_CHECK(ReadHandler, handler) type_check;

  async_completion<ReadHandler,
    void (std::error_code, std::size_t)> init(handler);

  detail::start_read_buffer_sequence_op(s, buffers,
      std::experimental::net::v1::buffer_sequence_begin(buffers), transfer_all(),
      init.completion_handler);

  return init.result.get();
}

namespace detail
{
  template <typename AsyncReadStream, typename DynamicBuffer,
      typename CompletionCondition, typename ReadHandler>
  class read_dynbuf_op
    : detail::base_from_completion_cond<CompletionCondition>
  {
  public:
    template <typename BufferSequence>
    read_dynbuf_op(AsyncReadStream& stream,
        NET_TS_MOVE_ARG(BufferSequence) buffers,
        CompletionCondition completion_condition, ReadHandler& handler)
      : detail::base_from_completion_cond<
          CompletionCondition>(completion_condition),
        stream_(stream),
        buffers_(NET_TS_MOVE_CAST(BufferSequence)(buffers)),
        start_(0),
        total_transferred_(0),
        handler_(NET_TS_MOVE_CAST(ReadHandler)(handler))
    {
    }

#if defined(NET_TS_HAS_MOVE)
    read_dynbuf_op(const read_dynbuf_op& other)
      : detail::base_from_completion_cond<CompletionCondition>(other),
        stream_(other.stream_),
        buffers_(other.buffers_),
        start_(other.start_),
        total_transferred_(other.total_transferred_),
        handler_(other.handler_)
    {
    }

    read_dynbuf_op(read_dynbuf_op&& other)
      : detail::base_from_completion_cond<CompletionCondition>(other),
        stream_(other.stream_),
        buffers_(NET_TS_MOVE_CAST(DynamicBuffer)(other.buffers_)),
        start_(other.start_),
        total_transferred_(other.total_transferred_),
        handler_(NET_TS_MOVE_CAST(ReadHandler)(other.handler_))
    {
    }
#endif // defined(NET_TS_HAS_MOVE)

    void operator()(const std::error_code& ec,
        std::size_t bytes_transferred, int start = 0)
    {
      std::size_t max_size, bytes_available;
      switch (start_ = start)
      {
        case 1:
        max_size = this->check_for_completion(ec, total_transferred_);
        bytes_available = std::min<std::size_t>(
              std::max<std::size_t>(512,
                buffers_.capacity() - buffers_.size()),
              std::min<std::size_t>(max_size,
                buffers_.max_size() - buffers_.size()));
        for (;;)
        {
          stream_.async_read_some(buffers_.prepare(bytes_available),
              NET_TS_MOVE_CAST(read_dynbuf_op)(*this));
          return; default:
          total_transferred_ += bytes_transferred;
          buffers_.commit(bytes_transferred);
          max_size = this->check_for_completion(ec, total_transferred_);
          bytes_available = std::min<std::size_t>(
                std::max<std::size_t>(512,
                  buffers_.capacity() - buffers_.size()),
                std::min<std::size_t>(max_size,
                  buffers_.max_size() - buffers_.size()));
          if ((!ec && bytes_transferred == 0) || bytes_available == 0)
            break;
        }

        handler_(ec, static_cast<const std::size_t&>(total_transferred_));
      }
    }

  //private:
    AsyncReadStream& stream_;
    DynamicBuffer buffers_;
    int start_;
    std::size_t total_transferred_;
    ReadHandler handler_;
  };

  template <typename AsyncReadStream, typename DynamicBuffer,
      typename CompletionCondition, typename ReadHandler>
  inline void* networking_ts_handler_allocate(std::size_t size,
      read_dynbuf_op<AsyncReadStream, DynamicBuffer,
        CompletionCondition, ReadHandler>* this_handler)
  {
    return networking_ts_handler_alloc_helpers::allocate(
        size, this_handler->handler_);
  }

  template <typename AsyncReadStream, typename DynamicBuffer,
      typename CompletionCondition, typename ReadHandler>
  inline void networking_ts_handler_deallocate(void* pointer, std::size_t size,
      read_dynbuf_op<AsyncReadStream, DynamicBuffer,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_alloc_helpers::deallocate(
        pointer, size, this_handler->handler_);
  }

  template <typename AsyncReadStream, typename DynamicBuffer,
      typename CompletionCondition, typename ReadHandler>
  inline bool networking_ts_handler_is_continuation(
      read_dynbuf_op<AsyncReadStream, DynamicBuffer,
        CompletionCondition, ReadHandler>* this_handler)
  {
    return this_handler->start_ == 0 ? true
      : networking_ts_handler_cont_helpers::is_continuation(
          this_handler->handler_);
  }

  template <typename Function, typename AsyncReadStream,
      typename DynamicBuffer, typename CompletionCondition,
      typename ReadHandler>
  inline void networking_ts_handler_invoke(Function& function,
      read_dynbuf_op<AsyncReadStream, DynamicBuffer,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_invoke_helpers::invoke(
        function, this_handler->handler_);
  }

  template <typename Function, typename AsyncReadStream,
      typename DynamicBuffer, typename CompletionCondition,
      typename ReadHandler>
  inline void networking_ts_handler_invoke(const Function& function,
      read_dynbuf_op<AsyncReadStream, DynamicBuffer,
        CompletionCondition, ReadHandler>* this_handler)
  {
    networking_ts_handler_invoke_helpers::invoke(
        function, this_handler->handler_);
  }
} // namespace detail

#if !defined(GENERATING_DOCUMENTATION)

template <typename AsyncReadStream, typename DynamicBuffer,
    typename CompletionCondition, typename ReadHandler, typename Allocator>
struct associated_allocator<
    detail::read_dynbuf_op<AsyncReadStream,
      DynamicBuffer, CompletionCondition, ReadHandler>,
    Allocator>
{
  typedef typename associated_allocator<ReadHandler, Allocator>::type type;

  static type get(
      const detail::read_dynbuf_op<AsyncReadStream,
        DynamicBuffer, CompletionCondition, ReadHandler>& h,
      const Allocator& a = Allocator()) NET_TS_NOEXCEPT
  {
    return associated_allocator<ReadHandler, Allocator>::get(h.handler_, a);
  }
};

template <typename AsyncReadStream, typename DynamicBuffer,
    typename CompletionCondition, typename ReadHandler, typename Executor>
struct associated_executor<
    detail::read_dynbuf_op<AsyncReadStream,
      DynamicBuffer, CompletionCondition, ReadHandler>,
    Executor>
{
  typedef typename associated_executor<ReadHandler, Executor>::type type;

  static type get(
      const detail::read_dynbuf_op<AsyncReadStream,
        DynamicBuffer, CompletionCondition, ReadHandler>& h,
      const Executor& ex = Executor()) NET_TS_NOEXCEPT
  {
    return associated_executor<ReadHandler, Executor>::get(h.handler_, ex);
  }
};

#endif // !defined(GENERATING_DOCUMENTATION)

template <typename AsyncReadStream,
    typename DynamicBuffer, typename ReadHandler>
inline NET_TS_INITFN_RESULT_TYPE(ReadHandler,
    void (std::error_code, std::size_t))
async_read(AsyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    NET_TS_MOVE_ARG(ReadHandler) handler,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  return async_read(s,
      NET_TS_MOVE_CAST(DynamicBuffer)(buffers),
      transfer_all(), NET_TS_MOVE_CAST(ReadHandler)(handler));
}

template <typename AsyncReadStream, typename DynamicBuffer,
    typename CompletionCondition, typename ReadHandler>
inline NET_TS_INITFN_RESULT_TYPE(ReadHandler,
    void (std::error_code, std::size_t))
async_read(AsyncReadStream& s,
    NET_TS_MOVE_ARG(DynamicBuffer) buffers,
    CompletionCondition completion_condition,
    NET_TS_MOVE_ARG(ReadHandler) handler,
    typename enable_if<
      is_dynamic_buffer<typename decay<DynamicBuffer>::type>::value
    >::type*)
{
  // If you get an error on the following line it means that your handler does
  // not meet the documented type requirements for a ReadHandler.
  NET_TS_READ_HANDLER_CHECK(ReadHandler, handler) type_check;

  async_completion<ReadHandler,
    void (std::error_code, std::size_t)> init(handler);

  detail::read_dynbuf_op<AsyncReadStream,
    typename decay<DynamicBuffer>::type,
      CompletionCondition, NET_TS_HANDLER_TYPE(
        ReadHandler, void (std::error_code, std::size_t))>(
          s, NET_TS_MOVE_CAST(DynamicBuffer)(buffers),
            completion_condition, init.completion_handler)(
              std::error_code(), 0, 1);

  return init.result.get();
}

} // inline namespace v1
} // namespace net
} // namespace experimental
} // namespace std

#include <experimental/__net_ts/detail/pop_options.hpp>

#endif // NET_TS_IMPL_READ_HPP
