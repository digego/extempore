#include <gtest/gtest.h>

#include <atomic>
#include <cerrno>
#include <chrono>
#include <thread>
#include <vector>

#include "EXTThread.h"
#include "PriorityQueue.h"
#include "Task.h"

// EXTThread::setPriority reads UNIV::SAMPLE_RATE on macOS; the rest of UNIV.cpp
// drags in the Scheme interpreter, so provide the one global here instead.
// UNIV.h (via EXTThread.h) already declares it, so this is only the definition.
namespace extemp {
namespace UNIV {
uint32_t SAMPLE_RATE = 44100;
}
}  // namespace extemp

using extemp::EXTThread;

namespace {

struct Body {
    std::atomic<bool> entered{false};
    std::atomic<bool> exited{false};
    std::atomic<bool> sawSelf{false};
    EXTThread* self = nullptr;
};

void* spin_until_stopped(void* arg) {
    auto* body = static_cast<Body*>(arg);
    body->entered = true;
    body->sawSelf = (EXTThread::activeThread() == body->self) && body->self->isCurrentThread();
    while (!body->self->stopRequested()) {
        std::this_thread::sleep_for(std::chrono::microseconds(200));
    }
    body->exited = true;
    return nullptr;
}

void wait_for(std::atomic<bool>& flag) {
    for (int i = 0; i < 5000 && !flag; ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}

}  // namespace

TEST(EXTThread, KillIsObservedThroughStopRequested) {
    Body body;
    EXTThread thread(spin_until_stopped, &body, "unit");
    body.self = &thread;
    EXPECT_FALSE(thread.isRunning());
    EXPECT_EQ(thread.join(), EINVAL) << "joining an unstarted thread is an error, not a hang";
    ASSERT_EQ(thread.start(), 0);
    EXPECT_TRUE(thread.isRunning());
    wait_for(body.entered);
    ASSERT_TRUE(body.entered);
    EXPECT_TRUE(body.sawSelf) << "activeThread() must be the EXTThread inside its body";
    EXPECT_FALSE(thread.isCurrentThread()) << "the test thread is not the worker";
    EXPECT_FALSE(body.exited);
    EXPECT_EQ(thread.kill(), 0);
    EXPECT_EQ(thread.join(), 0);
    EXPECT_TRUE(body.exited);
    EXPECT_FALSE(thread.isRunning());
    EXPECT_EQ(thread.join(), EINVAL) << "second join reports the thread is gone";
}

TEST(EXTThread, DestructorRequestsStopAndJoins) {
    Body body;
    {
        EXTThread thread(spin_until_stopped, &body, "unit");
        body.self = &thread;
        ASSERT_EQ(thread.start(), 0);
        wait_for(body.entered);
        ASSERT_TRUE(body.entered);
        // Leaving scope with the worker still running must not terminate();
        // the body polls stopRequested() so the destructor's join returns.
    }
    EXPECT_TRUE(body.exited);
}

TEST(EXTThread, StartTwiceIsRejected) {
    Body body;
    EXTThread thread(spin_until_stopped, &body, "unit");
    body.self = &thread;
    ASSERT_EQ(thread.start(), 0);
    EXPECT_EQ(thread.start(), EINVAL);
    thread.kill();
    EXPECT_EQ(thread.join(), 0);
}

// The scheduler pops tasks from a PriorityQueue keyed on start time, so the
// order tasks come out is the order they fire. The scheduler thread itself
// needs the audio device and Scheme runtime linked in, so it is not driven here.
TEST(PriorityQueue, TasksPopInStartTimeOrder) {
    extemp::PriorityQueue<extemp::TaskI> queue;
    const std::vector<uint64_t> times{500, 10, 300, 10, 4000, 1};
    for (auto t : times) {
        queue.add(new extemp::Task<int>(t, 0, nullptr, int(t)));
    }
    EXPECT_EQ(queue.size(), times.size());
    uint64_t previous = 0;
    while (auto* task = queue.pop()) {
        EXPECT_GE(task->getStartTime(), previous);
        previous = task->getStartTime();
        delete task;
    }
    EXPECT_EQ(previous, 4000u);
    EXPECT_EQ(queue.peek(), nullptr);
}

// Two callbacks scheduled for the same instant run in the order they were
// scheduled. (The key used to end in the task's address, so the order changed
// from run to run with the allocator's whim.)
TEST(PriorityQueue, EqualStartTimesPopInTheOrderAdded) {
    extemp::PriorityQueue<extemp::TaskI> queue;
    std::vector<extemp::TaskI*> made;
    for (int i = 0; i < 64; ++i) {
        made.push_back(new extemp::Task<int>(i % 2 ? 1000 : 2000, 0, nullptr, i));
    }
    // added in an order unrelated to their addresses
    for (int i = 63; i >= 0; --i) {
        queue.add(made[size_t(i)]);
    }
    std::vector<int> order;
    while (auto* task = queue.pop()) {
        order.push_back(static_cast<extemp::Task<int>*>(task)->getArg());
        delete task;
    }
    std::vector<int> expected;
    for (int i = 63; i >= 0; i -= 2) {
        expected.push_back(i);  // the odd ones, due at 1000
    }
    for (int i = 62; i >= 0; i -= 2) {
        expected.push_back(i);  // then the even ones, due at 2000
    }
    EXPECT_EQ(order, expected);
}
