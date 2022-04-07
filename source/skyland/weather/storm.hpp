#include "environment.hpp"
#include "skyland/island.hpp"



namespace skyland::weather
{



class Storm : public CleanEnvironment
{
private:
    Vec2<u16> raindrops_[6];

public:

    void update(Platform& pfrm, App& app, Microseconds delta);


    void display(Platform& pfrm, App& app) override;


    const char* music() const override;


    Platform::Screen::Shader shader(App& app) const override;
};




}
