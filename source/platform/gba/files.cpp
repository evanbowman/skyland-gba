

extern const unsigned char file_test[];
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

    {"scripts", "test.lisp", file_test},
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
