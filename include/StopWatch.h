#ifndef StopWatch_H_
#define StopWatch_H_

#include "nanotime.h"

class StopWatch
{
private:
    nanotime m_startTime;
public:
    StopWatch(bool Start = true): m_startTime((Start) ? getNanotime() : nanotime()) {
    }

    void set(nanotime Start) {
        m_startTime = Start;
    }
    nanotime reset() {
        return (m_startTime = getNanotime());
    }
    nanotime split() {
        auto end(getNanotime());
        auto dur(end - m_startTime);
        m_startTime = end;
        return dur;
    }
    nanotime elapsed() const {
        return getNanotime() - m_startTime;
    }
};

#endif /* StopWatch_H_ */
