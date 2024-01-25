//
// Created by Jack Xu on 1/20/24.
//

#ifndef CP_HEADERS_IO_STRING_H
#define CP_HEADERS_IO_STRING_H

#include <iostream>
#include <vector>
#include <sstream>

using namespace std;

// scan n integers

template<typename T>
T read() {
    T t;
    cin >> t;
    return t;
}

template<typename T>
vector<T> read_vec(size_t num) {
    vector<T> v;
    v.resize(num);
    T t;
    for(size_t idx = 0; idx < num; idx++) {
        cin >> v[idx];
    }
    return v;
}

template<typename T>
vector<vector<T>> read_grid(size_t rows, size_t cols) {
    vector<vector<T>> v(rows, vector<T>(cols, {}));
    for(size_t row = 0; row < rows; row++) {
        for(size_t col = 0; col < cols; col++) {
            cin >> v[row][col];
        }
    }
    return v;
}

template<typename... Args>
void print(Args&&... args) {
    (cout << ... << args) << "\n";
}

template<typename T>
void print_vec(const vector<T>& v, optional<string> delimiter = nullopt, ostream& os = cout) {
    for(size_t idx = 0; idx < v.size(); idx++) {
        if (idx) {
            if (delimiter.has_value()) os << *delimiter;
            else os << " ";
        }
        os << v[idx];
    }
    os << "\n";
}

template<typename T>
void print_grid(const vector<vector<T>>& g, optional<string> delimiter = nullopt, ostream& os = cout) {
    for(size_t row = 0; row < g.size(); row++) {
        print_vec<T>(g[row], delimiter, os);
    }
}

template<typename... Args>
string join(Args&&... args) {
    stringstream ss;
    (ss << ... << args);
    return ss.str();
}


#endif //CP_HEADERS_IO_STRING_H
