#pragma once

#include <nlohmann/json.hpp>
#include <test.h>

void to_json(nlohmann::json &j, Test *test);
void to_json(nlohmann::json &j, const Result &result);
void to_json(nlohmann::json &j, const Result *result);
