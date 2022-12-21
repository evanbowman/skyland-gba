#pragma once


#define TIMEPOINT(NAME)                                                        \
    [[maybe_unused]] const auto NAME = pfrm.delta_clock().sample()
