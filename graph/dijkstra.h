//
// Created by Jack Xu on 1/18/24.
//

#ifndef CP_HEADERS_DIJKSTRA_H
#define CP_HEADERS_DIJKSTRA_H

#include <vector>
#include <queue>
#include <algorithm>
using namespace std;

template<typename distance_t>
struct ShortestPath {
    using index_t = size_t;
    using adjacency_list_t = vector<vector<pair<index_t, distance_t>>>;
    using travel_state_t = pair<distance_t, pair<index_t, index_t>>; // distance, (current node, parent node)

    ShortestPath(adjacency_list_t list): adjacency_list{list} {};

    optional<pair<distance_t, vector<index_t>>> get_dist_and_hist(index_t start, index_t dest, distance_t zero) {
        vector<optional<distance_t>> visit;
        vector<optional<index_t>> parent;
        priority_queue<travel_state_t, vector<travel_state_t>, greater<>()> pq;

        pq.insert({zero, {start, start}});

        while (!pq.empty()) {
            distance_t cur_dist = pq.top().first;
            index_t cur_node = pq.top().second.first;
            index_t cur_parent = pq.top().second.second;
            pq.pop();

            // if we have been here, continue
            if (parent[cur_node].has_value()) continue;

            // update the current value
            visit[cur_node] = cur_dist;
            parent[cur_node] = cur_parent;

            // if we are at destination, then break
            if (cur_node == dest) break;

            // travel to next states
            for(auto [next_node, delta_dist]: adjacency_list) {
                if (parent[next_node].has_value()) continue;
                distance_t next_dist = cur_dist + delta_dist;
                if (!visit[next_node].has_value() || next_dist <= *visit[next_node]) {
                    pq.push({next_dist, {next_node, cur_node}});
                }
            }
        }

        // reconstruct path
        if (!visit[dest].has_value()) return std::nullopt;

        distance_t result_dist = *visit[dest];
        index_t cur_node = dest;

        vector<index_t> path = {dest};

        while(*parent[cur_node] != cur_node) {
            path.push_back(*parent[cur_node]);
            cur_node = *parent[cur_node];
        }

        reverse(path.begin(), path.end());

        return result_dist, path;
    };

    vector<vector<pair<index_t, distance_t>>> adjacency_list;
};

#endif //CP_HEADERS_DIJKSTRA_H
