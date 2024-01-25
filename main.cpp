#include <iostream>
#include <vector>
//#include "itertools/itertools_ext.h"
//#include "io/io_string.h"
//#include "math/eval.h"


//
// Created by Jack Xu on 1/20/24.
//

// This file consists of utilities used for the generic nature of the
// iterable wrapper classes.  As such, the contents of this file should be
// considered UNDOCUMENTED and is subject to change without warning.  This
// also applies to the name of the file.  No user code should include
// this file directly.

#include <cassert>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace iter {
    namespace impl {
        namespace get_iters {
            template <typename T, std::size_t N>
            T* get_begin_impl(T (&array)[N], int) {
                return array;
            }

            template <typename T, typename I = decltype(std::declval<T&>().begin())>
            I get_begin_impl(T& r, int) {
                return r.begin();
            }

            template <typename T, typename I = decltype(begin(std::declval<T&>()))>
            I get_begin_impl(T& r, long) {
                return begin(r);
            }

            template <typename T>
            auto get_begin(T& t) -> decltype(get_begin_impl(std::declval<T&>(), 42)) {
                return get_begin_impl(t, 42);
            }

            template <typename T, std::size_t N>
            T* get_end_impl(T (&array)[N], int) {
                return array + N;
            }

            template <typename T, typename I = decltype(std::declval<T&>().end())>
            I get_end_impl(T& r, int) {
                return r.end();
            }

            template <typename T, typename I = decltype(end(std::declval<T&>()))>
            I get_end_impl(T& r, long) {
                return end(r);
            }

            template <typename T>
            auto get_end(T& t) -> decltype(get_end_impl(std::declval<T&>(), 42)) {
                return get_end_impl(t, 42);
            }
        }
        using get_iters::get_begin;
        using get_iters::get_end;

        template <typename T>
        struct type_is {
            using type = T;
        };

        template <typename T>
        using AsConst = decltype(std::as_const(std::declval<T&>()));

        template <typename T>
        using iterator_type = decltype(get_begin(std::declval<T&>()));

        template <typename Container>
        using const_iterator_type = decltype(get_begin(
                std::declval<const std::remove_reference_t<Container>&>()));

        template <typename Container>
        using iterator_deref = decltype(*std::declval<iterator_type<Container>&>());

        template <typename Container>
        using const_iterator_deref =
                decltype(*std::declval<const iterator_type<Container>&>());

        template <typename Container>
        using const_iterator_type_deref =
                decltype(*std::declval<const_iterator_type<Container>&>());

        template <typename Container>
        using iterator_traits_deref =
                std::remove_reference_t<iterator_deref<Container>>;

        template <typename T, typename = void>
        struct IsIterable : std::false_type {};

        template <typename T>
        struct IsIterable<T, std::void_t<iterator_type<T>>> : std::true_type {};

        template <typename T>
        constexpr bool is_iterable = IsIterable<T>::value;

        struct Identity {
            template <typename T>
            const T& operator()(const T& t) const {
                return t;
            }
        };

        namespace detail {
            template <typename T, typename = void>
            struct ArrowHelper {
                using type = void;
                void operator()(T&) const noexcept {}
            };

            template <typename T>
            struct ArrowHelper<T*, void> {
                using type = T*;
                constexpr type operator()(T* t) const noexcept {
                    return t;
                }
            };

            template <typename T>
            struct ArrowHelper<T,
                    std::void_t<decltype(std::declval<T&>().operator->())>> {
                using type = decltype(std::declval<T&>().operator->());
                type operator()(T& t) const {
                    return t.operator->();
                }
            };

            template <typename T>
            using arrow = typename detail::ArrowHelper<T>::type;
        }

        template <typename C>
        using iterator_arrow = detail::arrow<iterator_type<C>>;

        template <typename T>
        detail::arrow<T> apply_arrow(T& t) {
            return detail::ArrowHelper<T>{}(t);
        }

        template <typename T>
        class ArrowProxy {
        private:
            using TPlain = typename std::remove_reference<T>::type;
            T obj;

        public:
            constexpr ArrowProxy(T&& in_obj) : obj(std::forward<T>(in_obj)) {}

            TPlain* operator->() {
                return &obj;
            }
        };

        template <typename, typename = void>
        struct is_random_access_iter : std::false_type {};

        template <typename T>
        struct is_random_access_iter<T,
                std::enable_if_t<
                        std::is_same<typename std::iterator_traits<T>::iterator_category,
                                std::random_access_iterator_tag>::value>> : std::true_type {};

        template <typename T>
        using has_random_access_iter = is_random_access_iter<iterator_type<T>>;

        template <typename InputIt, typename Distance = std::size_t>
        void dumb_advance_unsafe(InputIt& iter, Distance distance) {
            for (Distance i(0); i < distance; ++i) {
                ++iter;
            }
        }

        template <typename Iter, typename EndIter, typename Distance>
        void dumb_advance_impl(
                Iter& iter, const EndIter& end, Distance distance, std::false_type) {
            for (Distance i(0); i < distance && iter != end; ++i) {
                ++iter;
            }
        }

        template <typename Iter, typename EndIter, typename Distance>
        void dumb_advance_impl(
                Iter& iter, const EndIter& end, Distance distance, std::true_type) {
            if (static_cast<Distance>(end - iter) < distance) {
                iter = end;
            } else {
                iter += distance;
            }
        }

        template <typename Iter, typename EndIter, typename Distance = std::size_t>
        void dumb_advance(Iter& iter, const EndIter& end, Distance distance) {
            dumb_advance_impl(iter, end, distance, is_random_access_iter<Iter>{});
        }

        template <typename ForwardIt, typename Distance = std::size_t>
        ForwardIt dumb_next(ForwardIt it, Distance distance = 1) {
            dumb_advance_unsafe(it, distance);
            return it;
        }

        template <typename ForwardIt, typename Distance = std::size_t>
        ForwardIt dumb_next(
                ForwardIt it, const ForwardIt& end, Distance distance = 1) {
            dumb_advance(it, end, distance);
            return it;
        }

        template <typename Container, typename Distance = std::size_t>
        Distance dumb_size(Container&& container) {
            Distance d{0};
            auto end_it = get_end(container);
            for (auto it = get_begin(container); it != end_it; ++it) {
                ++d;
            }
            return d;
        }

        template <typename... Ts>
        struct are_same : std::true_type {};

        template <typename T, typename U, typename... Ts>
        struct are_same<T, U, Ts...>
                : std::integral_constant<bool,
                        std::is_same<T, U>::value && are_same<T, Ts...>::value> {};

        template <typename T>
        class DerefHolder {
        private:
            static_assert(!std::is_lvalue_reference<T>::value,
                          "Non-lvalue-ref specialization used for lvalue ref type");
            using TPlain = std::remove_reference_t<T>;

            std::optional<TPlain> item_p_;

        public:
            using reference = TPlain&;
            using pointer = TPlain*;

            static constexpr bool stores_value = true;

            DerefHolder() = default;

            reference get() {
                assert(item_p_.has_value());
                return *item_p_;
            }

            pointer get_ptr() {
                assert(item_p_.has_value());
                return &item_p_.value();
            }

            void reset(T&& item) {
                item_p_.emplace(std::move(item));
            }

            explicit operator bool() const {
                return static_cast<bool>(item_p_);
            }
        };

        template <typename T>
        class DerefHolder<T&> {
        public:
            using reference = T&;
            using pointer = T*;

        private:
            pointer item_p_{};

        public:
            static constexpr bool stores_value = false;

            DerefHolder() = default;

            reference get() {
                assert(item_p_);
                return *item_p_;
            }

            pointer get_ptr() {
                assert(item_p_);
                return item_p_;
            }

            void reset(reference item) {
                item_p_ = &item;
            }

            explicit operator bool() const {
                return item_p_ != nullptr;
            }
        };

        template <typename ItTool>
        struct Pipeable {
            template <typename T>
            friend decltype(auto) operator|(T&& x, const Pipeable& p) {
                return static_cast<const ItTool&>(p)(std::forward<T>(x));
            }
        };

        template <template <typename> class ItImpl>
        struct IterToolFn : Pipeable<IterToolFn<ItImpl>> {
            template <typename T, typename... Ts>
            ItImpl<T> operator()(T&& t, Ts... ts) const {
                return {std::forward<T>(t), std::move(ts)...};
            }
        };

        template <typename F>
        struct PipeableAndBindFirst : Pipeable<F> {
        protected:
            template <typename T>
            struct FnPartial : Pipeable<FnPartial<T>> {
                mutable T stored_arg;
                constexpr FnPartial(T in_t) : stored_arg(in_t) {}

                template <typename Container>
                auto operator()(Container&& container) const {
                    return F{}(stored_arg, std::forward<Container>(container));
                }
            };

        public:
            template <typename T, typename = std::enable_if_t<!is_iterable<T>>>
            FnPartial<std::decay_t<T>> operator()(T&& t) const {
                return {std::forward<T>(t)};
            }
        };

        template <typename F, typename DefaultT>
        struct PipeableAndBindOptionalSecond : Pipeable<F> {
        protected:
            template <typename T>
            struct FnPartial : Pipeable<FnPartial<T>> {
                mutable T stored_arg;
                constexpr FnPartial(T in_t) : stored_arg(in_t) {}

                template <typename Container>
                auto operator()(Container&& container) const {
                    return F{}(std::forward<Container>(container), stored_arg);
                }
            };

        public:
            template <typename T, typename = std::enable_if_t<!is_iterable<T>>>
            FnPartial<std::decay_t<T>> operator()(T&& t) const {
                return {std::forward<T>(t)};
            }

            template <typename Container,
                    typename = std::enable_if_t<is_iterable<Container>>>
            auto operator()(Container&& container) const {
                return static_cast<const F&>(*this)(
                        std::forward<Container>(container), DefaultT{});
            }
        };

        template <template <typename, typename> class ItImpl, typename DefaultT>
        struct IterToolFnOptionalBindFirst
                : PipeableAndBindFirst<IterToolFnOptionalBindFirst<ItImpl, DefaultT>> {
        private:
            using Base =
                    PipeableAndBindFirst<IterToolFnOptionalBindFirst<ItImpl, DefaultT>>;

        protected:
            template <typename Container>
            auto operator()(Container&& container, std::false_type) const {
                return static_cast<const Base&>(*this)(
                        std::forward<Container>(container));
            }

            template <typename Container>
            auto operator()(Container&& container, std::true_type) const {
                return (*this)(DefaultT{}, std::forward<Container>(container));
            }

        public:
            template <typename T>
            auto operator()(T&& t) const {
                return (*this)(std::forward<T>(t), IsIterable<T>{});
            }

            template <typename T, typename Container,
                    typename = std::enable_if_t<is_iterable<Container>>>
            ItImpl<T, Container> operator()(T func, Container&& container) const {
                return {std::move(func), std::forward<Container>(container)};
            }
        };

        template <template <typename, typename> class ItImpl, typename DefaultT>
        struct IterToolFnOptionalBindSecond
                : Pipeable<IterToolFnOptionalBindSecond<ItImpl, DefaultT>> {
        private:
            // T is whatever is being held for later use
            template <typename T>
            struct FnPartial : Pipeable<FnPartial<T>> {
                mutable T stored_arg;
                constexpr FnPartial(T in_t) : stored_arg(in_t) {}

                template <typename Container>
                auto operator()(Container&& container) const {
                    return IterToolFnOptionalBindSecond{}(
                            std::forward<Container>(container), stored_arg);
                }
            };

        public:
            template <typename Container, typename T>
            ItImpl<Container, T> operator()(Container&& container, T func) const {
                return {std::forward<Container>(container), std::move(func)};
            }

            template <typename T, typename = std::enable_if_t<!is_iterable<T>>>
            FnPartial<std::decay_t<T>> operator()(T&& func) const {
                return {std::forward<T>(func)};
            }

            template <typename Container,
                    typename = std::enable_if_t<is_iterable<Container>>>
            auto operator()(Container&& container) const {
                return (*this)(std::forward<Container>(container), DefaultT{});
            }
        };

        template <template <typename> class ItImpl>
        struct IterToolFnBindSizeTSecond {  // NOTE not pipeable
        private:
            using Size = std::size_t;
            struct FnPartial : Pipeable<FnPartial> {
                Size sz{};
                constexpr FnPartial(Size in_sz) : sz{in_sz} {}

                template <typename Container>
                auto operator()(Container&& container) const {
                    return IterToolFnBindSizeTSecond{}(
                            std::forward<Container>(container), sz);
                }
            };

        public:
            FnPartial operator()(Size sz) const {
                return {sz};
            }

            template <typename Container,
                    typename = std::enable_if_t<is_iterable<Container>>>
            ItImpl<Container> operator()(Container&& container, Size sz) const {
                return {std::forward<Container>(container), sz};
            }
        };
    }
}

#include <iterator>
#include <type_traits>
#include <utility>

namespace iter {
    namespace impl {
        template <typename T, typename = void>
        struct HasConstDeref : std::false_type {};

        template <typename T>
        struct HasConstDeref<T, std::void_t<decltype(*std::declval<const T&>())>>
                : std::true_type {};

        template <typename TopIter>
        class IteratorIterator {
            template <typename> friend class IteratorIterator;
            using Diff = std::ptrdiff_t;
            static_assert(
                    std::is_same<
                            typename std::iterator_traits<TopIter>::iterator_category,
                            std::random_access_iterator_tag>::value,
                    "IteratorIterator only works with random access iterators");

        private:
            TopIter sub_iter;

        public:
            using iterator_category = std::random_access_iterator_tag;
            using value_type = std::remove_reference_t<decltype(**std::declval<TopIter>())>;
            using difference_type = std::ptrdiff_t;
            using pointer = value_type*;
            using reference = value_type&;
            IteratorIterator() = default;
            IteratorIterator(const TopIter& it) : sub_iter{it} {}

            const TopIter& get() const {
                return sub_iter;
            }

            template <typename T>
            bool operator==(const IteratorIterator<T>& other) const {
                return !(*this != other);
            }

            template <typename T>
            bool operator!=(const IteratorIterator<T>& other) const {
                return this->sub_iter != other.sub_iter;
            }

            IteratorIterator& operator++() {
                ++this->sub_iter;
                return *this;
            }

            IteratorIterator operator++(int) {
                auto ret = *this;
                ++*this;
                return ret;
            }

            IteratorIterator& operator--() {
                --this->sub_iter;
                return *this;
            }

            IteratorIterator operator--(int) {
                auto ret = *this;
                --*this;
                return ret;
            }

            auto operator*() const -> decltype(**sub_iter) {
                return **this->sub_iter;
            }

            auto operator-> () const -> decltype(*sub_iter) {
                return *this->sub_iter;
            }

            IteratorIterator& operator+=(Diff n) {
                this->sub_iter += n;
                return *this;
            }

            IteratorIterator operator+(Diff n) const {
                auto it = *this;
                it += n;
                return it;
            }

            friend IteratorIterator operator+(Diff n, IteratorIterator it) {
                it += n;
                return it;
            }

            IteratorIterator& operator-=(Diff n) {
                this->sub_iter -= n;
                return *this;
            }

            IteratorIterator operator-(Diff n) const {
                auto it = *this;
                it -= n;
                return it;
            }

            Diff operator-(const IteratorIterator& rhs) const {
                return this->sub_iter - rhs.sub_iter;
            }

            auto operator[](Diff idx) const -> decltype(*sub_iter[idx]) {
                return *sub_iter[idx];
            }

            bool operator<(const IteratorIterator& rhs) const {
                return this->sub_iter < rhs.sub_iter;
            }

            bool operator>(const IteratorIterator& rhs) const {
                return this->sub_iter > rhs.sub_iter;
            }

            bool operator<=(const IteratorIterator& rhs) const {
                return this->sub_iter <= rhs.sub_iter;
            }

            bool operator>=(const IteratorIterator& rhs) const {
                return this->sub_iter >= rhs.sub_iter;
            }
        };

        template <typename Container>
        class IterIterWrapper {
        private:
            Container container;

            using contained_iter = typename Container::value_type;
            using size_type = typename Container::size_type;
            using iterator = IteratorIterator<typename Container::iterator>;
            using const_iterator =
                    IteratorIterator<typename Container::const_iterator>;
            using reverse_iterator =
                    IteratorIterator<typename Container::reverse_iterator>;
            using const_reverse_iterator =
                    IteratorIterator<typename Container::const_reverse_iterator>;

            template <typename U = Container, typename = void>
            struct ConstAtTypeOrVoid : type_is<void> {};

            template <typename U>
            struct ConstAtTypeOrVoid<U,
                    std::void_t<decltype(*std::declval<const U&>().at(0))>>
                    : type_is<decltype(*std::declval<const U&>().at(0))> {};

            using const_at_type_or_void_t = typename ConstAtTypeOrVoid<>::type;

            template <typename U = Container, typename = void>
            struct ConstIndexTypeOrVoid : type_is<void> {};

            template <typename U>
            struct ConstIndexTypeOrVoid<U,
                    std::void_t<decltype(*std::declval<const U&>()[0])>>
                    : type_is<decltype(*std::declval<const U&>()[0])> {};

            using const_index_type_or_void_t = typename ConstIndexTypeOrVoid<>::type;

        public:
            IterIterWrapper() = default;

            explicit IterIterWrapper(size_type sz) : container(sz) {}

            IterIterWrapper(size_type sz, const contained_iter& val)
                    : container(sz, val) {}

            auto at(size_type pos) -> decltype(*container.at(pos)) {
                return *container.at(pos);
            }

            auto at(size_type pos) const -> const_at_type_or_void_t {
                return *container.at(pos);
            }

            auto operator[](size_type pos) noexcept(noexcept(*container[pos]))
            -> decltype(*container[pos]) {
                return *container[pos];
            }

            auto operator[](size_type pos) const noexcept(noexcept(*container[pos]))
            -> const_index_type_or_void_t {
                return *container[pos];
            }

            bool empty() const noexcept {
                return container.empty();
            }

            size_type size() const noexcept {
                return container.size();
            }

            iterator begin() noexcept {
                return {container.begin()};
            }

            iterator end() noexcept {
                return {container.end()};
            }

            const_iterator begin() const noexcept {
                return {container.begin()};
            }

            const_iterator end() const noexcept {
                return {container.end()};
            }

            const_iterator cbegin() const noexcept {
                return {container.cbegin()};
            }

            const_iterator cend() const noexcept {
                return {container.cend()};
            }

            reverse_iterator rbegin() noexcept {
                return {container.rbegin()};
            }

            reverse_iterator rend() noexcept {
                return {container.rend()};
            }

            const_reverse_iterator rbegin() const noexcept {
                return {container.rbegin()};
            }

            const_reverse_iterator rend() const noexcept {
                return {container.rend()};
            }

            const_reverse_iterator crbegin() const noexcept {
                return {container.rbegin()};
            }

            const_reverse_iterator crend() const noexcept {
                return {container.rend()};
            }

            Container& get() noexcept {
                return container;
            }

            const Container& get() const noexcept {
                return container;
            }
        };
    }
}

#include <cassert>
#include <functional>
#include <variant>

namespace iter {
    namespace impl {
        template <typename Container>
        using iterator_end_type = decltype(get_end(std::declval<Container&>()));

        template <typename SubIter, typename SubEnd>
        class IteratorWrapperImpl;

        template <typename Container, bool same_types>
        struct IteratorWrapperImplType;

        template <typename Container>
        struct IteratorWrapperImplType<Container, true>
                : type_is<iterator_type<Container>> {};

    template <typename Container>
    struct IteratorWrapperImplType<Container, false>
            : type_is<IteratorWrapperImpl<iterator_type<Container>,
              iterator_end_type<Container>>> {};

template <typename Container>
using IteratorWrapper = typename IteratorWrapperImplType<Container,
        std::is_same_v<impl::iterator_type<Container>,
        impl::iterator_end_type<Container>>>::type;
}
}

template <typename SubIter, typename SubEnd>
class iter::impl::IteratorWrapperImpl {
private:
    static_assert(!std::is_same_v<SubIter, SubEnd>);
    SubIter& sub_iter() {
        auto* sub = std::get_if<SubIter>(&sub_iter_or_end_);
        assert(sub);
        return *sub;
    }

    const SubIter& sub_iter() const {
        auto* sub = std::get_if<SubIter>(&sub_iter_or_end_);
        assert(sub);
        return *sub;
    }

    std::variant<SubIter, SubEnd> sub_iter_or_end_;

public:
    IteratorWrapperImpl() : IteratorWrapperImpl(SubIter{}) {}

    IteratorWrapperImpl(SubIter&& it) : sub_iter_or_end_{std::move(it)} {}

    IteratorWrapperImpl(SubEnd&& it) : sub_iter_or_end_(std::move(it)) {}

    IteratorWrapperImpl& operator++() {
        ++sub_iter();
        return *this;
    }

    decltype(auto) operator*() {
        return *sub_iter();
    }

    decltype(auto) operator*() const {
        return *sub_iter();
    }

    decltype(auto) operator-> () {
        return apply_arrow(sub_iter());
    }

    decltype(auto) operator-> () const {
        return apply_arrow(sub_iter());
    }

    bool operator!=(const IteratorWrapperImpl& other) const {
        constexpr static struct : std::not_equal_to<void> {
            bool operator()(const SubEnd&, const SubEnd&) const {
                return false;
            }
            using std::not_equal_to<void>::operator();
        } not_equal;
        return std::visit(not_equal, sub_iter_or_end_, other.sub_iter_or_end_);
    }

    bool operator==(const IteratorWrapperImpl& other) const {
        return !(*this != other);
    }
};

namespace iter {
    namespace impl {
        namespace detail {
            template <typename... Ts>
            std::tuple<iterator_deref<Ts>...> iterator_tuple_deref_helper(
                    const std::tuple<Ts...>&);

            template <typename... Ts>
            std::tuple<IteratorWrapper<Ts>...> iterator_tuple_type_helper(
                    const std::tuple<Ts...>&);

            template <typename... Ts>
            std::tuple<iterator_deref<const std::remove_reference_t<Ts>>...>
            const_iterator_tuple_deref_helper(const std::tuple<Ts...>&);

            template <typename... Ts>
            std::tuple<IteratorWrapper<const std::remove_reference_t<Ts>>...>
            const_iterator_tuple_type_helper(const std::tuple<Ts...>&);
        }

        template <typename TupleType>
        using iterator_tuple_type =
                decltype(detail::iterator_tuple_type_helper(std::declval<TupleType>()));

        template <typename TupleType>
        using const_iterator_tuple_type = decltype(
        detail::const_iterator_tuple_type_helper(std::declval<TupleType>()));

        template <typename TupleType>
        using iterator_deref_tuple = decltype(
        detail::iterator_tuple_deref_helper(std::declval<TupleType>()));

        template <typename TupleType>
        using const_iterator_deref_tuple = decltype(
        detail::const_iterator_tuple_deref_helper(std::declval<TupleType>()));

        template <typename... Ts>
        void absorb(Ts&&...) {}
    }
}

#include <cassert>
#include <exception>
#include <iterator>
#include <type_traits>

namespace iter {
    namespace impl {
        template <typename T>
        class Range;
    }

    template <typename T>
    constexpr impl::Range<T> range(T) noexcept;
    template <typename T>
    constexpr impl::Range<T> range(T, T) noexcept;
    template <typename T>
    constexpr impl::Range<T> range(T, T, T) noexcept;
}

namespace iter {
    namespace detail {
        template <typename T, bool IsFloat = std::is_floating_point<T>::value>
        class RangeIterData;

        template <typename T>
        class RangeIterData<T, false> {
        private:
            T value_{};
            T step_{};

        public:
            constexpr RangeIterData() noexcept = default;
            constexpr RangeIterData(T in_value, T in_step) noexcept
                    : value_{in_value}, step_{in_step} {}

            constexpr T value() const noexcept {
                return value_;
            }

            constexpr T step() const noexcept {
                return step_;
            }

            void inc() noexcept {
                value_ += step_;
            }

            constexpr bool operator==(const RangeIterData& other) const noexcept {
                return value_ == other.value_;
            }

            constexpr bool operator!=(const RangeIterData& other) const noexcept {
                return !(*this == other);
            }
        };

        template <typename T>
        class RangeIterData<T, true> {
        private:
            T start_{};
            T value_{};
            T step_{};
            std::size_t steps_taken_{};

        public:
            constexpr RangeIterData() noexcept = default;
            constexpr RangeIterData(T in_start, T in_step) noexcept
                    : start_{in_start}, value_{in_start}, step_{in_step} {}

            constexpr T value() const noexcept {
                return value_;
            }

            constexpr T step() const noexcept {
                return step_;
            }

            void inc() noexcept {
                ++steps_taken_;
                value_ = start_ + (step_ * steps_taken_);
            }

            constexpr bool operator==(const RangeIterData& other) const noexcept {
                return (value_ < other.value_ ? other.value_ - value_
                                              : value_ - other.value_)
                       < step_;
            }

            constexpr bool operator!=(const RangeIterData& other) const noexcept {
                return !(*this == other);
            }
        };
    }
}

template <typename T>
class iter::impl::Range {
    template <typename U>
    friend constexpr Range<U> iter::range(U) noexcept;
    template <typename U>
    friend constexpr Range<U> iter::range(U, U) noexcept;
    template <typename U>
    friend constexpr Range<U> iter::range(U, U, U) noexcept;

private:
    const T start_;
    const T stop_;
    const T step_;

    constexpr Range(T stop) noexcept : start_{0}, stop_{stop}, step_{1} {}

    constexpr Range(T start, T stop, T step = 1) noexcept
            : start_{start}, stop_{stop}, step_{step} {}

    static constexpr bool is_within_range(
            T val, T stop_val, [[maybe_unused]] T step_val) {
        if constexpr (std::is_unsigned<T>{}) {
            return val < stop_val;
        } else {
            return !(step_val > 0 && val >= stop_val)
                   && !(step_val < 0 && val <= stop_val);
        }
    }

public:
    constexpr T start() const noexcept {
        return start_;
    }

    constexpr T stop() const noexcept {
        return stop_;
    }

    constexpr T step() const noexcept {
        return step_;
    }

    constexpr T operator[](std::size_t index) const noexcept {
        return start() + (step() * index);
    }

    constexpr std::size_t size() const noexcept {
        static_assert(!std::is_floating_point_v<T>,
                      "range size() not supperted with floating point types");
        if (!is_within_range(start(), stop(), step())) {
            return 0;
        }

        auto diff = stop() - start();
        auto res = diff / step();
        assert(res >= 0);
        auto result = static_cast<std::size_t>(res);
        if (diff % step()) {
            ++result;
        }
        return result;
    }

    class Iterator {
    private:
        iter::detail::RangeIterData<T> data;
        bool is_end{};

        static bool not_equal_to_impl(
                const Iterator& lhs, const Iterator& rhs) noexcept {
            assert(!lhs.is_end);
            assert(rhs.is_end);
            return is_within_range(
                    lhs.data.value(), rhs.data.value(), lhs.data.step());
        }

        static bool not_equal_to_end(
                const Iterator& lhs, const Iterator& rhs) noexcept {
            if (rhs.is_end) {
                return not_equal_to_impl(lhs, rhs);
            }
            return not_equal_to_impl(rhs, lhs);
        }

    public:
        using iterator_category = std::forward_iterator_tag;
        using value_type = T;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type*;
        using reference = value_type;

        constexpr Iterator() noexcept = default;

        constexpr Iterator(T in_value, T in_step, bool in_is_end) noexcept
                : data(in_value, in_step), is_end{in_is_end} {}

        constexpr T operator*() const noexcept {
            return data.value();
        }

        constexpr ArrowProxy<T> operator->() const noexcept {
            return {**this};
        }

        Iterator& operator++() noexcept {
            data.inc();
            return *this;
        }

        Iterator operator++(int) noexcept {
            auto ret = *this;
            ++*this;
            return ret;
        }

        bool operator!=(const Iterator& other) const noexcept {
            if (is_end && other.is_end) {
                return false;
            }

            if (!is_end && !other.is_end) {
                return data != other.data;
            }
            return not_equal_to_end(*this, other);
        }

        bool operator==(const Iterator& other) const noexcept {
            return !(*this != other);
        }
    };

    constexpr Iterator begin() const noexcept {
        return {start_, step_, false};
    }

    constexpr Iterator end() const noexcept {
        return {stop_, step_, true};
    }
};

template <typename T>
constexpr iter::impl::Range<T> iter::range(T stop_) noexcept {
    return {stop_};
}

template <typename T>
constexpr iter::impl::Range<T> iter::range(T start_, T stop_) noexcept {
    return {start_, stop_};
}

template <typename T>
constexpr iter::impl::Range<T> iter::range(
        T start_, T stop_, T step_) noexcept {
    return step_ == T(0) ? impl::Range<T>{0}
                         : impl::Range<T>{start_, stop_, step_};
}

#include <iterator>
#include <type_traits>
#include <vector>

namespace iter {
    namespace impl {
        template <typename Container>
        class Combinator;

        using CombinationsFn = IterToolFnBindSizeTSecond<Combinator>;
    }
    constexpr impl::CombinationsFn combinations{};
}

template <typename Container>
class iter::impl::Combinator {
private:
    Container container_;
    std::size_t length_;

    friend CombinationsFn;

    Combinator(Container&& container, std::size_t length)
            : container_(std::forward<Container>(container)), length_{length} {}

    template <typename T>
    using IndexVector = std::vector<iterator_type<T>>;
    template <typename T>
    using CombIteratorDeref = IterIterWrapper<IndexVector<T>>;

public:
    Combinator(Combinator&&) = default;
    template <typename ContainerT>
    class Iterator {
    private:
        template <typename>
        friend class Iterator;
        constexpr static const int COMPLETE = -1;
        std::remove_reference_t<ContainerT>* container_p_;
        CombIteratorDeref<ContainerT> indices_;
        int steps_{};

    public:
        using iterator_category = std::input_iterator_tag;
        using value_type = CombIteratorDeref<ContainerT>;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type*;
        using reference = value_type&;

        Iterator(ContainerT& container, std::size_t n)
                : container_p_{&container}, indices_{n} {
            if (n == 0) {
                steps_ = COMPLETE;
                return;
            }
            size_t inc = 0;
            for (auto& iter : indices_.get()) {
                auto it = get_begin(*container_p_);
                dumb_advance(it, get_end(*container_p_), inc);
                if (it != get_end(*container_p_)) {
                    iter = it;
                    ++inc;
                } else {
                    steps_ = COMPLETE;
                    break;
                }
            }
        }

        static Iterator zero_length_end(ContainerT& container) {
            Iterator it{container, 0};
            it.steps_ = 0;
            return it;
        }

        CombIteratorDeref<ContainerT>& operator*() {
            return indices_;
        }

        CombIteratorDeref<ContainerT>* operator->() {
            return &indices_;
        }

        Iterator& operator++() {
            if (indices_.get().empty()) {
                // zero-length case.
                ++steps_;
                return *this;
            }
            for (auto iter = indices_.get().rbegin(); iter != indices_.get().rend();
                 ++iter) {
                ++(*iter);

                auto dist = std::distance(indices_.get().rbegin(), iter);

                if (!(dumb_next(*iter, dist) != get_end(*container_p_))) {
                    if ((iter + 1) != indices_.get().rend()) {
                        size_t inc = 1;
                        for (auto down = iter;; --down) {
                            (*down) = dumb_next(*(iter + 1), 1 + inc);
                            ++inc;
                            if (down == indices_.get().rbegin()) break;
                        }
                    } else {
                        steps_ = COMPLETE;
                        break;
                    }
                } else {
                    break;
                }
            }
            if (steps_ != COMPLETE) {
                ++steps_;
            }
            return *this;
        }

        Iterator operator++(int) {
            auto ret = *this;
            ++*this;
            return ret;
        }

        template <typename T>
        bool operator!=(const Iterator<T>& other) const {
            return !(*this == other);
        }

        template <typename T>
        bool operator==(const Iterator<T>& other) const {
            return steps_ == other.steps_;
        }
    };

    Iterator<Container> begin() {
        return {container_, length_};
    }

    Iterator<Container> end() {
        if (length_ == 0) {
            return Iterator<Container>::zero_length_end(container_);
        }
        return {container_, 0};
    }

    Iterator<AsConst<Container>> begin() const {
        return {std::as_const(container_), length_};
    }

    Iterator<AsConst<Container>> end() const {
        if (length_ == 0) {
            return Iterator<AsConst<Container>>::zero_length_end(container_);
        }
        return {std::as_const(container_), 0};
    }
};

#include <array>
#include <iterator>
#include <tuple>
#include <utility>

namespace iter {
    namespace impl {
        template <typename TupleType, std::size_t... Is>
        class Productor;

        template <typename TupleType, std::size_t... Is>
        Productor<TupleType, Is...> product_impl(
                TupleType&& containers, std::index_sequence<Is...>);
    }
}

template <typename TupleType, std::size_t... Is>
class iter::impl::Productor {
    friend Productor iter::impl::product_impl<TupleType, Is...>(
            TupleType&&, std::index_sequence<Is...>);

private:
    TupleType containers_;

    Productor(TupleType&& containers) : containers_(std::move(containers)) {}

public:
    Productor(Productor&&) = default;

private:
    template <typename IterTupType>
    class IteratorData {
        IteratorData() = delete;
        static_assert(
                std::tuple_size<std::decay_t<IterTupType>>::value == sizeof...(Is),
                "tuple size != sizeof Is");

    public:
        template <std::size_t Idx>
        static bool equal(const IterTupType& lhs, const IterTupType& rhs) {
            return !(std::get<Idx>(lhs) != std::get<Idx>(rhs));
        }

        template <std::size_t Idx>
        static bool get_and_increment_with_wraparound(IterTupType& iters,
                                                      const IterTupType& begin_iters, const IterTupType& end_iters) {
            if (equal<Idx>(iters, end_iters)) {
                return false;
            }

            ++std::get<Idx>(iters);

            if (equal<Idx>(iters, end_iters)) {
                std::get<Idx>(iters) = std::get<Idx>(begin_iters);
                return false;
            }

            return true;
        }
        using IncFunc = bool (*)(
                IterTupType&, const IterTupType&, const IterTupType&);

        constexpr static std::array<IncFunc, sizeof...(Is)> incrementers{
                {get_and_increment_with_wraparound<Is>...}};
    };

    template <typename TupleTypeT, template <typename> class IteratorTuple,
            template <typename> class TupleDeref>
    class IteratorTempl {

    public:

        using IterTupType = IteratorTuple<TupleTypeT>;
        IterTupType iters_;
        IterTupType begin_iters_;
        IterTupType end_iters_;

    public:
        using iterator_category = std::input_iterator_tag;
        using value_type = TupleDeref<TupleTypeT>;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type*;
        using reference = value_type;

        IteratorTempl(IteratorTuple<TupleTypeT>&& iters,
                      IteratorTuple<TupleTypeT>&& end_iters)
                : iters_(std::move(iters)),
                  begin_iters_(iters_),
                  end_iters_(std::move(end_iters)) {}

        IteratorTempl& operator++() {
            static constexpr int NUM_ELEMENTS = sizeof...(Is);
            bool performed_increment = false;
            for (int i = NUM_ELEMENTS - 1; i >= 0; --i) {
                if (IteratorData<IterTupType>::incrementers[i](
                        iters_, begin_iters_, end_iters_)) {
                    performed_increment = true;
                    break;
                }
            }
            if (!performed_increment) {
                iters_ = end_iters_;
            }
            return *this;
        }

        IteratorTempl operator++(int) {
            auto ret = *this;
            ++*this;
            return ret;
        }

        template <typename T, template <typename> class IT,
                template <typename> class TD>
        bool operator!=(const IteratorTempl<T, IT, TD>& other) const {
            if constexpr (sizeof...(Is) == 0) {
                return false;
            } else {
                return (... && (std::get<Is>(iters_) != std::get<Is>(other.iters_)));
            }
        }

        template <typename T, template <typename> class IT,
                template <typename> class TD>
        bool operator==(const IteratorTempl<T, IT, TD>& other) const {
            return !(*this != other);
        }

        TupleDeref<TupleTypeT> operator*() {
            return {(*std::get<Is>(iters_))...};
        }

        auto operator->() -> ArrowProxy<decltype(**this)> {
            return {**this};
        }
    };

    using Iterator =
            IteratorTempl<TupleType, iterator_tuple_type, iterator_deref_tuple>;
    using ConstIterator = IteratorTempl<AsConst<TupleType>,
    const_iterator_tuple_type, const_iterator_deref_tuple>;

public:
    Iterator begin() {
        return {{get_begin(std::get<Is>(containers_))...},
                {get_end(std::get<Is>(containers_))...}};
    }

    Iterator end() {
        return {{get_end(std::get<Is>(containers_))...},
                {get_end(std::get<Is>(containers_))...}};
    }

    ConstIterator begin() const {
        return {{get_begin(std::as_const(std::get<Is>(containers_)))...},
                {get_end(std::as_const(std::get<Is>(containers_)))...}};
    }

    ConstIterator end() const {
        return {{get_end(std::as_const(std::get<Is>(containers_)))...},
                {get_end(std::as_const(std::get<Is>(containers_)))...}};
    }
};

namespace iter::impl {
    template <typename TupleType, std::size_t... Is>
    Productor<TupleType, Is...> product_impl(
            TupleType&& containers, std::index_sequence<Is...>) {
        return {std::move(containers)};
    }
}

namespace iter {
    template <typename... Containers>
    decltype(auto) product(Containers&&... containers) {
        return impl::product_impl(
                std::tuple<Containers...>(std::forward<Containers>(containers)...),
                std::index_sequence_for<Containers...>{});
    }

    constexpr std::array<std::tuple<>, 1> product() {
        return {{}};
    }
}

namespace iter::impl {
    template <std::size_t... Is, typename Container>
    decltype(auto) product_repeat(
            std::index_sequence<Is...>, Container&& container) {
        return product(((void)Is, Container(container))...);
    }

    template <std::size_t... Is, typename Container>
    decltype(auto) product_repeat(
            std::index_sequence<Is...>, Container& container) {
        return product(((void)Is, container)...);
    }

    template <std::size_t... Is, typename Container>
    decltype(auto) product_repeat(
            std::index_sequence<Is...>, const Container& container) {
        return product(((void)Is, container)...);
    }
}

namespace iter {
    template <std::size_t N, typename Container>
    decltype(auto) product(Container&& container) {
        return impl::product_repeat(
                std::make_index_sequence<N>{}, std::forward<Container>(container));
    }
}

#include <functional>
#include <initializer_list>
#include <iterator>
#include <tuple>
#include <type_traits>
#include <utility>

namespace iter {
    namespace impl {
        template <typename Index, typename Elem>
        using EnumBasePair = std::pair<Index, Elem>;

        template <typename Index, typename Elem>
        class EnumIterYield : public EnumBasePair<Index, Elem> {
            using BasePair = EnumBasePair<Index, Elem>;
            using BasePair::BasePair;

        public:
            typename BasePair::first_type index = BasePair::first;
            typename BasePair::second_type element = BasePair::second;
        };

        template <typename Container, typename Index>
        class Enumerable;

        using EnumerateFn = IterToolFnOptionalBindSecond<Enumerable, std::size_t>;
    }
    constexpr impl::EnumerateFn enumerate{};
}

namespace std {
    template <typename Index, typename Elem>
    struct tuple_size<iter::impl::EnumIterYield<Index, Elem>>
            : public tuple_size<iter::impl::EnumBasePair<Index, Elem>> {};

    template <std::size_t N, typename Index, typename Elem>
    struct tuple_element<N, iter::impl::EnumIterYield<Index, Elem>>
            : public tuple_element<N, iter::impl::EnumBasePair<Index, Elem>> {};
}

template <typename Container, typename Index>
class iter::impl::Enumerable {
private:
    Container container_;
    const Index start_;

    friend EnumerateFn;

    Enumerable(Container&& container, Index start)
            : container_(std::forward<Container>(container)), start_{start} {}

public:
    Enumerable(Enumerable&&) = default;

    template <typename T>
    using IterYield = EnumIterYield<Index, iterator_deref<T>>;

    template <typename ContainerT>
    class Iterator {
    private:
        template <typename>
        friend class Iterator;
        IteratorWrapper<ContainerT> sub_iter_;
        Index index_;

    public:
        using iterator_category = std::input_iterator_tag;
        using value_type = IterYield<ContainerT>;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type*;
        using reference = value_type&;

        Iterator(IteratorWrapper<ContainerT>&& sub_iter, Index start)
                : sub_iter_{std::move(sub_iter)}, index_{start} {}

        IterYield<ContainerT> operator*() {
            return {index_, *sub_iter_};
        }

        ArrowProxy<IterYield<ContainerT>> operator->() {
            return {**this};
        }

        Iterator& operator++() {
            ++sub_iter_;
            ++index_;
            return *this;
        }

        Iterator operator++(int) {
            auto ret = *this;
            ++*this;
            return ret;
        }

        template <typename T>
        bool operator!=(const Iterator<T>& other) const {
            return sub_iter_ != other.sub_iter_;
        }

        template <typename T>
        bool operator==(const Iterator<T>& other) const {
            return !(*this != other);
        }
    };

    Iterator<Container> begin() {
        return {get_begin(container_), start_};
    }

    Iterator<Container> end() {
        return {get_end(container_), start_};
    }

    Iterator<AsConst<Container>> begin() const {
        return {get_begin(std::as_const(container_)), start_};
    }

    Iterator<AsConst<Container>> end() const {
        return {get_end(std::as_const(container_)), start_};
    }
};

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

#include <iostream>
#include <vector>
#include <sstream>

using namespace std;

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

using namespace std;

int main() {
    vector<string> operators = {"+", "-", "*", "/"};

    unordered_map<int, string> solutions;

    for(auto [op1, op2, op3]: iter::product<3>(operators)) {
        string operations = join("4 ", op1, " 4 ", op2, " 4 ", op3, " 4");

        int result = evaluate(operations);

        if (solutions.count(result) == 0)
            solutions[result] = operations;
    }

    size_t n_queries = read<size_t>();

    while(n_queries--) {
        int query = read<int>();
        if (solutions.count(query)) {
            print(solutions[query], " = ", query);
        } else {
            print("no solution");
        }
    }
}
