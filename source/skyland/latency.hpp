#pragma once


#define TIMEPOINT(NAME)                                                        \
    [[maybe_unused]] const auto NAME = PLATFORM.delta_clock().sample()
