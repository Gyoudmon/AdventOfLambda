#pragma once

#include <gydm/game.hpp>

#include "sprite/ulpc.hpp"

#include <iostream>
#include <fstream>

namespace WarGrey::AoC {
    static const char* aoc_ext = ".aoc";
    
    /* Constants */
    static const char* title0_fmt = "C++ 降临节: %s";
    static const char* title_fmt = "C++ 降临节: [第%d天] %s";
    static const char* puzzle_fmt = "%s: %d";
    static const char* unknown_fmt = "%s: (未知)";
    
    /* Fonts */
    static const int answer_fontsize = GYDM::generic_font_size(GYDM::FontSize::x_large);
}
