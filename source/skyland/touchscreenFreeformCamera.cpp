#include "touchscreenFreeformCamera.hpp"
#include "skyland.hpp"



namespace skyland {



void TouchscreenFreeformCamera::update(Platform& pfrm,
                                       App& app,
                                       Island& target,
                                       const Vec2<u8>& cursor_loc,
                                       Microseconds delta,
                                       bool near)
{
    if (auto loc = app.player().touch_current(pfrm)) {
        if (not scroll_locus_) {
            scroll_locus_ = *loc;
        }

        if (not app.player().touch_held(milliseconds(200))) {
            return;
        }

        auto offset = scroll_locus_->cast<s32>() - loc->cast<s32>();
        auto view = pfrm.screen().get_view();

        auto result = view_center_ + offset.cast<Float>();
        result.y = clamp(result.y, -56.f, 0.f);
        result.x = clamp(result.x, -50.f, [&] {
            if (app.opponent_island()) {
                return std::min(264.f,
                                (app.opponent_island()->get_position().x +
                                 app.opponent_island()->terrain().size() * 8.f) - 100.f);
            } else {
                return app.player_island().get_position().x - 40.f;
            }
        }());

        view.set_center(result);

        pfrm.screen().set_view(view);

    } else {
        scroll_locus_.reset();
        view_center_ = pfrm.screen().get_view().get_center();
    }

    // auto freeform_view = pfrm.screen().get_view();

    // Camera::update(pfrm, app, target, cursor_loc, delta, near);

    // auto regular_camera_middle =
}



void TouchscreenFreeformCamera::reset_default(App& app)
{
    app.camera().emplace<Camera>();
}



}
