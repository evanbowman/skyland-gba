#include "bulkTimer.hpp"
#include "platform/platform.hpp"



// NOTE: This bulkTimer class allows subscribers to schedule future updates
// based in microsecond timeouts. The idea being that many objects in the game
// just sit around incrementing timers, and we can avoid a lot of virtual method
// calls for every frame by having a scheduler manage future updates and invoke
// one polymorphic function when a timer expires.
//
// In the future, I would also like to restructure this class such that timers
// are grouped together by timeout window, so that whole generations of timers
// don't need to be updated until the most recent generation of
// soon-to-be-expired timers finishes. But this logic is somewhat complex,
// particularly when we need to correctly account for rewind behavior, so I
// haven't optimized bulkTimer too much.
//
// Still, even just moving the timer updates out of Room::update()
// implementations and into a loop in BulkTimer::update() results in significant
// performance improvments, because a tight loop that just iterates over a list
// and decrements counters plays really nice with the gba cartridge instruction
// prefetch logic.



namespace skyland
{



void BulkTimer::rewind(Platform& pfrm, App& app, Microseconds elapsed_delta)
{
    if (elapsed_delta == 0) {
        return;
    }

    auto list = scheduled_;
    scheduled_ = nullptr;

    while (list) {
        const auto next = list->next_;

        list->clock_ += elapsed_delta;
        if (list->clock_ >= list->interval_) {
            list->clock_ -= list->interval_;

            // Always relink?
            list->next_ = scheduled_;
            scheduled_ = list;
        } else {
            // Not finished counting up, re-link the timer to the update
            // list.
            list->next_ = scheduled_;
            scheduled_ = list;
        }

        list = next;
    }
}



void BulkTimer::update(Platform& pfrm, App& app, Microseconds elapsed_delta)
{
    if (elapsed_delta == 0) {
        return;
    }

    auto list = scheduled_;
    scheduled_ = nullptr;

    while (list) {
        const auto next = list->next_;

        list->clock_ -= elapsed_delta;
        if (list->clock_ <= 0) {
            list->timer_expired(pfrm, app);
        } else {
            // Not finished counting down, re-link the timer to the update
            // list.
            list->next_ = scheduled_;
            scheduled_ = list;
        }

        list = next;
    }
}



void BulkTimer::schedule(Timer* subscriber, Microseconds timeout)
{
    subscriber->next_ = scheduled_;
    subscriber->clock_ += timeout;
    subscriber->interval_ = timeout;
    scheduled_ = subscriber;
}



void BulkTimer::deschedule(Timer* subscriber)
{
    if (scheduled_ == subscriber) {
        scheduled_ = scheduled_->next_;
    } else {
        auto list = scheduled_;
        auto prev = list;
        while (list) {
            if (list == subscriber) {
                // Unlink element
                prev->next_ = list->next_;
                return;
            }
            prev = list;
            list = list->next_;
        }
    }
}



}
