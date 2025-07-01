#pragma once

#include <nlohmann/json.hpp>
#include <tmx_private.h>

void to_json( nlohmann::json& j, const Map& m);
void to_json(nlohmann::json &j, const Layer &l);
void to_json(nlohmann::json &j, const LayerChunk &lc);
void to_json(nlohmann::json &j, const Tile &t) ;
// void from_json(const nlohmann::json::json& j, Map& p);
