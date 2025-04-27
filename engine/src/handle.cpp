#include <tmx_private.h>
#include <handle.h>
#include <vector>
#include <unordered_map>

std::vector<Resource*> store;
std::unordered_map<Resource *, int> mapping;

int toHandle(Resource *r) {
  if(mapping.contains(r)) return mapping.at(r);

  store.push_back(r);
  int handle = store.size() -1;
  mapping[r] = handle;

  return handle;
}

Resource* fromHandle(int handle) {
  return store.at(handle);
}
