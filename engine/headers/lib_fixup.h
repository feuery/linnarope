#pragma once

#include <string>

std::string get_file_contents(const char *filename);
void spit(const char *filename, std::string &contents);
