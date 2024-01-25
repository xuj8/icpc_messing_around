//
// Created by Jack Xu on 1/20/24.
//

#ifndef CP_HEADERS_COLLECTIONS_H
#define CP_HEADERS_COLLECTIONS_H

#include <map>
#include <string>

using namespace std;

template<typename T>
struct collection {
    collection(const vector<T>& objs) {
        for(const auto& obj: objs) counts[obj]++;
    }

    map<T, int>::iterator begin() {return counts.begin();}
    map<T, int>::iterator end() {return counts.end();}

    map<T, int> counts;
};

template<>
struct collection<string> {
    collection(const string& str) {
        for(const auto& c: str) counts[c]++;
    }

    map<char, int>::iterator begin() {return counts.begin();}
    map<char, int>::iterator end() {return counts.end();}

    map<char, int> counts;
};



#endif //CP_HEADERS_COLLECTIONS_H
