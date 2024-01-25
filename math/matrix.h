//
// Created by Jack Xu on 1/23/24.
//

#ifndef CP_HEADERS_MATRIX_H
#define CP_HEADERS_MATRIX_H


#include <vector>
#include <optional>

using namespace std;

template<typename NumType>
struct Matrix {

    struct RowAccessor {
        friend class Matrix<NumType>;
        NumType& operator[](size_t col_index) {

            Matrix& matrix_ref = matrix.get();

            if (col_index >= matrix_ref.cols)
                throw runtime_error("Error, accessing beyond number of cols");

            return matrix_ref.data[row_accessed*matrix_ref.cols + col_index];
        }

    private:
        RowAccessor(Matrix& _matrix, size_t _row_accessed): matrix{_matrix}, row_accessed{_row_accessed} {};
        reference_wrapper<Matrix<NumType>> matrix;
        size_t row_accessed;
    };

    Matrix(size_t _rows, size_t _cols): rows{_rows}, cols{_cols} {
        data = vector<NumType>(rows*cols, {});
    };

    Matrix(const vector<vector<NumType>>& _data): rows{_data.size()}, cols{_data[0].size()} {
        data = vector<NumType>(rows*cols, {});

        for(size_t row = 0; row < rows; row++)
            for(size_t col = 0; col < cols; col++)
                data[row*cols + col] = _data[row][col];
    }

    Matrix operator*(const Matrix& other) {
        if (cols != other.rows)
            throw runtime_error("Error, shape does not fit");

        Matrix result(rows, other.cols);
    };

    Matrix operator*(int scalar) const {
        Matrix result(rows, cols);

        for(size_t row = 0; row < rows; row++) {
            for(size_t col = 0; col < cols; col++) {
                result.data[row*cols + row] = data[row*cols + row] * scalar;
            }
        }

        return result;
    }

    Matrix operator*(double scalar) const {
        Matrix result(rows, cols);

        for(size_t row = 0; row < rows; row++) {
            for(size_t col = 0; col < cols; col++) {
                result.data[row*cols + row] = data[row*cols + row] * scalar;
            }
        }

        return result;
    }

    Matrix int_div(int scalar) const {
        Matrix result(rows, cols);

        for(size_t row = 0; row < rows; row++) {
            for(size_t col = 0; col < cols; col++) {
                result.data[row*cols + row] = data[row*cols + row] / scalar;
            }
        }

        return result;
    }

    Matrix operator+(const Matrix& other) const {
        if (cols != other.cols || rows != other.rows) {
            throw runtime_error("Error, shapes do not fit");
        }

        Matrix result(rows, cols);

        for(size_t row = 0; row < rows; row++) {
            for(size_t col = 0; col < cols; col++) {
                result.data[row*cols + row] = data[row*cols + row] + other.data[row*cols + row];
            }
        }

        return result;
    }

    Matrix& operator*=(const Matrix& other) {
        return *this = move((*this) * other);
    }

    Matrix& operator+=(const Matrix& other) {
        return *this = move((*this) + other);
    }

    Matrix T() const {
        Matrix result = Matrix(cols, rows);

        for(size_t row = 0; row < rows; row++) {
            for(size_t col = 0; col < cols; col++) {
                result.data[col*rows + row] = data[row*cols + col];
            }
        }

        return result;
    }

    Matrix exp(size_t power) const {
        if (rows != cols)
            throw runtime_error("Error, matrix not square");

        Matrix result = identity(rows, cols);
        Matrix cur_power = *this;

        while(power) {
            if (power % 2) {
                result *= cur_power;
            }

            cur_power *= cur_power;
            power >>= 1;
        }

        return result;
    };

    RowAccessor operator[](size_t row_accessed) {
        if (row_accessed >= rows)
            throw runtime_error("Error, accessing beyond number of rows");
        return RowAccessor(*this, row_accessed);
    }

    static Matrix identity(size_t _rows, optional<size_t> _cols_opt = nullopt) {
        size_t _cols;
        if (!_cols_opt.has_value())
            _cols = _rows;
        else
            _cols = *_cols_opt;

        Matrix result = Matrix(_rows, _cols);

        for(size_t idx = 0; idx < min(_rows, _cols); idx++) {
            result.data[idx*_cols + idx] = 1;
        }

        return result;
    }

    const size_t rows, cols;
private:
    vector<NumType> data;
};



#endif //CP_HEADERS_MATRIX_H
