#pragma once

#include "number/int.h"
#include <tuple>


using IrqState = std::pair<u16, u16>;
IrqState critical_section_enter();
void critical_section_exit(IrqState state);
