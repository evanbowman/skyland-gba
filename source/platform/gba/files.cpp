

extern const unsigned char file_newgame[];
//;
extern const unsigned char file_hostile[];
//;
extern const unsigned char file_test[];
//;
extern const unsigned char file_test2[];
//;
extern const unsigned char file_hostile_0[];
//;
extern const unsigned char file_hostile_0_0[];
//;
extern const unsigned char file_hostile_0_1[];
//;
extern const unsigned char file_english[];
//;
extern const unsigned char file_spanish[];
//;
extern const unsigned char file_chinese[];
//;
extern const unsigned char file_russian[];
//

static const struct {
    const char* root_;
    const char* name_;
    const unsigned char* data_;
} files[] = {

    {"scripts", "newgame.lisp", file_newgame},
    //;
    {"scripts", "hostile.lisp", file_hostile},
    //;
    {"scripts", "test.lisp", file_test},
    //;
    {"scripts", "test2.lisp", file_test2},
    //;
    {"scripts", "hostile_0.lisp", file_hostile_0},
    //;
    {"scripts", "hostile_0_0.lisp", file_hostile_0_0},
    //;
    {"scripts", "hostile_0_1.lisp", file_hostile_0_1},
    //;
    {"strings", "english.txt", file_english},
    //;
    {"strings", "spanish.txt", file_spanish},
    //;
    {"strings", "chinese.txt", file_chinese},
    //;
    {"strings", "russian.txt", file_russian},
    //
};
