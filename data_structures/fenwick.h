//
// Created by Jack Xu on 1/20/24.
//

#ifndef CP_HEADERS_FENWICK_H
#define CP_HEADERS_FENWICK_H

#include <vector>

using namespace std;

// sums are inclusive to index
template<typename T>
struct Fenwick {
    using index_t = int;
    using value_t = T;

    Fenwick(index_t _max_size) {
        data = vector<T>(_max_size + 2);
    }

    Fenwick(const vector<T>& values) {
        data = vector<T>(values.size()+2);
        for(size_t idx = 0; idx < values.size(); idx++) {
            add(idx, values[idx]);
        }
    }

    // DOES NOT SHIFT
    void update(index_t idx, value_t value) {
        T orig_val = sum(idx-1, idx);
        T delta_val = value - orig_val;
        add(idx, delta_val);
    }

    // SHIFTS
    void add(index_t idx, value_t delta) {
        idx += 1;
        while(idx < data.size()) {
            data[idx] += delta;
            idx += idx & -idx;
        }
    }

    // DOES NOT SHIFT
    T sum(index_t begin_idx, index_t end_idx) {
        return prefix_sum(end_idx) - prefix_sum(begin_idx-1);
    }

private:

    // SHIFTS
    T prefix_sum(index_t end_idx) {
        end_idx += 1;
        T answer = 0;
        while(end_idx > 0) {
            answer += data[end_idx];
            end_idx -= end_idx & -end_idx;
        }
        return answer;
    }

    vector<T> data;
};

#endif //CP_HEADERS_FENWICK_H
