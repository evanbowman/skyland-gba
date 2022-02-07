#pragma once



// Mainly intended for sharing integer variables with configuration scripts.



namespace skyland {



class SharedVariable {
public:
    SharedVariable(const char* name);
    SharedVariable(const char* name, int initial);


    SharedVariable(const SharedVariable&) = delete;


    ~SharedVariable();


    static SharedVariable* load(const char* name);


    void set(int value)
    {
        value_ = value;
    }


    int get() const
    {
        return value_;
    }


    operator int() const
    {
        return value_;
    }


private:
    const char* name_;
    SharedVariable* next_;
    int value_;
};



#define SHARED_VARIABLE(name) SharedVariable name(#name)



} // namespace skyland
