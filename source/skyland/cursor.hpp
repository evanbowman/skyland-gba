#pragma once


namespace skyland {


class Island;


class Cursor {
public:
    void set_frame_of_reference(Island* island)
    {
        frame_of_reference_ = island;
    }


private:
    Island* frame_of_reference_ = nullptr;
};


} // namespace skyland
