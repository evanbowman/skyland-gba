#include "compression.hpp"
#include "allocator.hpp"
#include "containers/vector.hpp"



void compress(const Vector<char>& input, Vector<char>& output)
{
    struct Window
    {
        Buffer<char, 1000> input_;
        Buffer<char, 1000> output_;
    };

    auto win = allocate_dynamic<Window>("compr-window");

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        if (win->input_.full()) {
            Platform::fatal("fixme: cannot compress for some reason");
            compress(win->input_, win->output_);
            for (char c : win->output_) {
                output.push_back(c);
            }
            win->input_.clear();
            win->output_.clear();
        }
        win->input_.push_back(*it);
    }

    if (not win->input_.empty()) {
        compress(win->input_, win->output_);
        for (char c : win->output_) {
            output.push_back(c);
        }
    }
}



void decompress(const Vector<char>& input, Vector<char>& output)
{
    struct Window
    {
        Buffer<char, 1000> input_;
        Buffer<char, 1000> output_;
    };

    auto win = allocate_dynamic<Window>("compr-window");

    for (auto it = input.begin(); it not_eq input.end(); ++it) {
        // NOTE: why size == 255? We want to make sure that the output buffer is
        // large enough to hold the decompressed stuff. A greater than 4x
        // compression ratio would be quite high for heatshrink...
        if (win->input_.size() == 255) {
            decompress(win->input_, win->output_);
            for (char c : win->output_) {
                output.push_back(c);
            }
            win->input_.clear();
            win->output_.clear();
        }
        win->input_.push_back(*it);
    }

    if (not win->input_.empty()) {
        decompress(win->input_, win->output_);
        for (char c : win->output_) {
            output.push_back(c);
        }
    }
}
