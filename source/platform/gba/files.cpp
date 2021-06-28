

extern const unsigned char file_newgame[];
//;
extern const unsigned char file_reset_hooks[];
//;
extern const unsigned char file_hostile[];
//;
extern const unsigned char file_hostile_0_0[];
//;
extern const unsigned char file_hostile_0_1[];
//;
extern const unsigned char file_hostile_0_2[];
//;
extern const unsigned char file_hostile_0_3[];
//;
extern const unsigned char file_hostile_0_4[];
//;
extern const unsigned char file_hostile_0_5[];
//;
extern const unsigned char file_hostile_0_6[];
//;
extern const unsigned char file_hostile_0_7[];
//;
extern const unsigned char file_hostile_1_0[];
//;
extern const unsigned char file_neutral[];
//;
extern const unsigned char file_neutral_0_0[];
//;
extern const unsigned char file_neutral_0_1[];
//;
extern const unsigned char file_neutral_0_2[];
//;
extern const unsigned char file_neutral_0_3[];
//;
extern const unsigned char file_neutral_1_0[];
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
    {"scripts", "reset_hooks.lisp", file_reset_hooks},
//;
    {"scripts", "hostile.lisp", file_hostile},
//;
    {"scripts", "hostile_0_0.lisp", file_hostile_0_0},
//;
    {"scripts", "hostile_0_1.lisp", file_hostile_0_1},
//;
    {"scripts", "hostile_0_2.lisp", file_hostile_0_2},
//;
    {"scripts", "hostile_0_3.lisp", file_hostile_0_3},
//;
    {"scripts", "hostile_0_4.lisp", file_hostile_0_4},
//;
    {"scripts", "hostile_0_5.lisp", file_hostile_0_5},
//;
    {"scripts", "hostile_0_6.lisp", file_hostile_0_6},
//;
    {"scripts", "hostile_0_7.lisp", file_hostile_0_7},
//;
    {"scripts", "hostile_1_0.lisp", file_hostile_1_0},
//;
    {"scripts", "neutral.lisp", file_neutral},
//;
    {"scripts", "neutral_0_0.lisp", file_neutral_0_0},
//;
    {"scripts", "neutral_0_1.lisp", file_neutral_0_1},
//;
    {"scripts", "neutral_0_2.lisp", file_neutral_0_2},
//;
    {"scripts", "neutral_0_3.lisp", file_neutral_0_3},
//;
    {"scripts", "neutral_1_0.lisp", file_neutral_1_0},
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
