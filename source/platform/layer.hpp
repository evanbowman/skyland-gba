#pragma once


enum class Layer {
    overlay,
    // NOTE: map_0 and map_1 were used by the legacy blind jump core, before the
    // codebases diverged. Should not be used!
    map_1,
    map_0,
    background,
    map_1_ext,
    map_0_ext,
};
