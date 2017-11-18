#include <algorithm>
#include <array>
#include <cassert>
#include <sstream>
#include <string>
#include <vector>

class TTranslator {
    using ui8 = uint8_t;
    using TVector = std::vector<ui8>;

public:
    std::string Translate(const std::string& from, const uint baseFrom, const uint baseTo) {
        uint powers[baseFrom] {0}; // logarithm[i] == log_{baseTo}(i)
        for (auto i = 0u, v = 1u; i < baseFrom; ++i) {
            powers[i] = v;
            if (i == v) {
                v *= baseTo;
            }
        }

        auto& baseFromMultiplier = Buf[6] = {};
        {
            auto power = baseTo;
            while (power < baseFrom) {
                const auto newPow = power * baseTo;

                if (newPow <= baseFrom) {
                    power = newPow;
                } else {
                    break;
                }
            }

            for (auto reminder = baseFrom;; power /= baseTo) {
                if (power) {
                    baseFromMultiplier.push_back(reminder / power);
                    reminder %= power;
                } else {
                    break;
                }
            }
        }

        auto& resVec = Buf[5] = {};

        auto& multiplier = Buf[0] = {1};

        for (auto it = from.rbegin(); it != from.rend(); ++it) {
            if (auto rem = Char2Digit(*it, baseFrom)) {
                Buf[1].clear();
                for (auto power = powers[rem]; power; power /= baseTo) {
                    Buf[1].push_back(rem / power);
                    rem %= power;
                }
            } else {
                Buf[1].assign(1, 0);
            }

            if (it != from.rbegin()) {
                Buf[2].clear();
                Multiply(multiplier, baseFromMultiplier, baseTo, Buf[2]);
                std::swap(multiplier, Buf[2]);

                Buf[2].clear();
                Multiply(multiplier, Buf[1], baseTo, Buf[2]);
            } else {
                std::swap(Buf[1], Buf[2]);
            }

            if (resVec.empty()) {
                std::swap(resVec, Buf[2]);
            } else {
                Sum(resVec, Buf[2], baseTo, Buf[1]);
                std::swap(resVec, Buf[1]);
            }
        }

        RemoveLeadingZeros(resVec);

        std::string res;
        res.resize(resVec.size());
        std::transform(resVec.begin(), resVec.end(), res.begin(), [] (const auto digit) {
            return Digit2Char(digit);
        });

        return res;
    }

private:
    static constexpr ui8 Char2Digit(char c, const uint base) noexcept {
        if (isdigit(c)) {
            const auto d = c - '0';
            assert(d < base);
            return d;
        }
        assert(isalpha(c));
        c = tolower(c);

        assert(c <= 'z' && c >= 'a');
        const auto d = 10 + (c - 'a');
        return d;
    }

    static constexpr char Digit2Char(const ui8 num) noexcept {
        if (num < 10) {
            return '0' + num;
        } else {
            return 'a' + (num - 10);
        }
    }

    void Sum(const TVector& first, const TVector& second, const uint base, TVector& res) {
        res.clear();
        res.reserve(first.size() + second.size());

        uint carry = 0;

        auto f = first.rbegin(), s = second.rbegin();
        const auto doIter = [&] (auto& v, auto& iter, const auto& str) {
            if (iter != str.rend()) {
                v += *iter;
                ++iter;
            }
            return iter != str.rend();
        };

        for (bool ok = true; ok;) {
            auto v = carry;
            ok = doIter(v, f, first);
            if (doIter(v, s, second)) {
                ok = true;
            }

            carry = v / base;
            res.push_back(v % base);
        };

        if (carry) {
            assert(carry < base);
            res.push_back(carry);
        }

        std::reverse(res.begin(), res.end());
    }

    void Multiply(const TVector& vec, const ui8 m, const uint base, TVector& res, const uint padding = 0) {
        res.clear();
        res.reserve(vec.size() + 1);

        uint carry = 0;
        for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
            carry += *it * m;
            res.push_back(carry % base);
            carry /= base;
            assert(carry < base);
        }

        if (carry) {
            assert(carry < base);
            res.push_back(carry);
        }

        std::reverse(res.begin(), res.end());
        res.insert(res.end(), padding, 0);
    }

    void RemoveLeadingZeros(TVector& vec) {
        const auto leadingZeros = std::find_if(vec.begin(), vec.end(), [&] (auto c) {
            return c != 0;
        });
        if (leadingZeros == vec.end()) {
            vec.assign(1, 0);
        } else {
            vec.erase(vec.begin(), leadingZeros);
        }
    }

    void Multiply(const TVector& first, const TVector& second, const uint base, TVector& res) {
        res.clear();

        for (auto i = 0u; i < first.size(); ++i) {
            Multiply(second, first[first.size() - 1 - i], base, Buf[3], i);
            if (i) {
                Sum(res, Buf[3], base, Buf[4]);
                std::swap(res, Buf[4]);
            } else {
                std::swap(res, Buf[3]);
            }
        }

        RemoveLeadingZeros(res);
    }

private:
    std::array<TVector, 7> Buf;
};

int main() {
    TTranslator tr;

    assert((tr.Translate("16", 10, 16) == "10"));
    assert((tr.Translate("241", 10, 16) == "f1"));
    assert((tr.Translate("17", 10, 16) == "11"));

    assert((tr.Translate("F1", 16, 10) == "241"));
    assert((tr.Translate("11", 16, 10) == "17"));
    assert((tr.Translate("11", 8, 10) == "9"));

    assert((tr.Translate("63", 10, 8) == "77"));
    assert((tr.Translate("9", 10, 8) == "11"));
    assert((tr.Translate("8", 10, 8) == "10"));
    assert((tr.Translate("63", 10, 8) == "77"));
    assert((tr.Translate("64", 10, 8) == "100"));
    assert((tr.Translate("65", 10, 8) == "101"));

    assert((tr.Translate("23", 10, 5) == "43"));

    assert((tr.Translate("17", 10, 2) == "10001"));

    srand(777);
    for (auto i = 0u; i < 1000u; ++i) {
        const auto num = rand();

        std::string expected;
        {
            std::ostringstream ss;
            ss << std::hex << num;
            expected = ss.str();
        }
        std::string original;
        {
            std::ostringstream ss;
            ss << num;
            original = ss.str();
        }

        assert(tr.Translate(original, 10, 16) == expected);
        assert(tr.Translate(expected, 16, 10) == original);
    }

    return EXIT_SUCCESS;
}
