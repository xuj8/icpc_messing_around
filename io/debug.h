//
// Created by Jack Xu on 1/19/24.
//

#ifndef CP_HEADERS_DEBUG_H
#define CP_HEADERS_DEBUG_H

#include <vector>
#include <iostream>
#include <optional>

using namespace std;

#ifdef DEBUG
#define debug(a) {cout << #a << " is " << (a) << endl;}
#define debug_vec(a) {cout << #a << ": "; for(auto elem:a) cout << elem << " "; cout << endl;}
#define debug_grid(a) {cout << #a << ":\n"; for(auto row:a) {for(auto elem: row) cout << elem << " "; cout << endl;}}
#define debug_simple(a) {cout << (a) << endl;}
#else
#define debug(a)
#define debug_vec(a)
#define debug_grid(a)
#define debug_simple(a)
#endif

#endif //CP_HEADERS_DEBUG_H
