//
// Created by Jack Xu on 1/20/24.
//

#ifndef CP_HEADERS_EVAL_H
#define CP_HEADERS_EVAL_H

#include <iostream>
#include <stack>
#include <string>
#include <cctype>
#include <cmath>

int _precedence(char op) {
    if (op == '+' || op == '-') return 1;
    if (op == '*' || op == '/' || op == '%') return 2;
    return 0;
}

int _apply_op(int a, int b, char op) {
    switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/': return a / b; // Integer division
        case '%': return a % b;
        default: return 0;
    }
}

int evaluate(const std::string& expression) {
    std::stack<int> values;
    std::stack<char> ops;

    for (int i = 0; i < expression.length(); i++) {
        if (expression[i] == ' ') continue;

        else if (expression[i] == '(') {
            ops.push(expression[i]);
        }

        else if (isdigit(expression[i])) {
            int val = 0;
            while (i < expression.length() && isdigit(expression[i])) {
                val = (val * 10) + (expression[i] - '0');
                i++;
            }
            values.push(val);
            i--;
        }

        else if (expression[i] == ')') {
            while (!ops.empty() && ops.top() != '(') {
                int val2 = values.top();
                values.pop();

                int val1 = values.top();
                values.pop();

                char op = ops.top();
                ops.pop();

                values.push(_apply_op(val1, val2, op));
            }
            if (!ops.empty()) ops.pop();
        }

        else {
            while (!ops.empty() && _precedence(ops.top()) >= _precedence(expression[i])) {
                int val2 = values.top();
                values.pop();

                int val1 = values.top();
                values.pop();

                char op = ops.top();
                ops.pop();

                values.push(_apply_op(val1, val2, op));
            }
            ops.push(expression[i]);
        }
    }

    while (!ops.empty()) {
        int val2 = values.top();
        values.pop();

        int val1 = values.top();
        values.pop();

        char op = ops.top();
        ops.pop();

        values.push(_apply_op(val1, val2, op));
    }

    return values.top();
}

#endif //CP_HEADERS_EVAL_H
