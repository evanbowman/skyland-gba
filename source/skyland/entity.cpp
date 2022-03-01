#include "entity.hpp"
#include "globals.hpp"



namespace skyland {



GlobalEntityListData::Pool& GlobalEntityListData::pool() const
{
    return std::get<SkylandGlobalData>(globals()).entity_node_pool_;
}



}
