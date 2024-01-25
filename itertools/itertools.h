//
// Created by Jack Xu on 1/20/24.
//

#ifndef CP_HEADERS_ITERTOOLS_H
#define CP_HEADERS_ITERTOOLS_H

#include <vector>

using namespace std;

class next_coords {
public:
    class Iterator {
    public:
        Iterator(int c1, int c2, bool diagonals, bool origin, bool end)
                : c1(c1), c2(c2), diagonals(diagonals), origin(origin), index(end ? -1 : 0) {}
        bool operator!=(const Iterator& other) const {
            return index != other.index;
        }

        Iterator& operator++() {
            if (index == -1)
                return *this;
            ++index;
            if (!diagonals) {
                if (index == 4) {
                    if (!origin) index = -1;
                    else index = 8;
                } else if (index > 4) {
                    index = -1;
                }
            } else {
                // have diagonals
                // if at 8, if have origin, OK, otherwise go to -1
                if (index == 8) {
                    if (!origin) index = -1;
                } else if (index > 8) {
                    index = -1;
                }
            }
            return *this;
        }

        std::tuple<int, int> operator*() const {
            static const int dc1[9] = {0, 0, 1, -1, 1, 1, -1, -1, 0};
            static const int dc2[9] = {1, -1, 0, 0, 1, -1, 1, -1, 0};
            if (index == -1)
                throw runtime_error("Error, at end");
            return {c1 + dc1[index], c2 + dc2[index]};
        }

    private:
        int c1, c2;
        bool diagonals, origin;
        int index;
        bool end;
    };

    next_coords(int c1, int c2, bool diagonals = false, bool origin = false)
            : c1(c1), c2(c2), diagonals(diagonals), origin(origin) {}

    Iterator begin() const {
        return Iterator(c1, c2, diagonals, origin, false);
    }

    Iterator end() const {
        return Iterator(c1, c2, diagonals, origin, true);
    }

private:
    int c1, c2;
    bool diagonals, origin;
};

template<typename T>
class enumerate_ref {
public:
    class Iterator {
    public:
        Iterator(int index, typename T::iterator it) : index(index), it(it) {}

        Iterator& operator++() {
            ++index;
            ++it;
            return *this;
        }

        bool operator!=(const Iterator& other) const {
            return it != other.it;
        }

        std::tuple<int, typename T::reference> operator*() {
            return forward_as_tuple(index, *it);
        }

    private:
        int index;
        typename T::iterator it;
    };

    enumerate_ref(T& container) : container(container) {}

    Iterator begin() {
        return Iterator(0, container.begin());
    }

    Iterator end() {
        return Iterator(container.size(), container.end());
    }

private:
    T& container;
};

template<typename T>
class enumerate_val {
public:
    class Iterator {
    public:
        Iterator(int index, typename T::iterator it) : index(index), it(it) {}

        Iterator& operator++() {
            ++index;
            ++it;
            return *this;
        }

        bool operator!=(const Iterator& other) const {
            return it != other.it;
        }

        std::tuple<int, typename T::value_type> operator*() {
            return forward_as_tuple(index, *it);
        }

    private:
        int index;
        typename T::iterator it;
    };

    enumerate_val(T& container) : container(container) {}

    Iterator begin() {
        return Iterator(0, container.begin());
    }

    Iterator end() {
        return Iterator(container.size(), container.end());
    }

private:
    T& container;
};



#endif //CP_HEADERS_ITERTOOLS_H
