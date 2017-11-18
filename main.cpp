include <algorithm>
#include <array>
#include <cassert>
#include <sstream>
#include <string>

class TTranslator {
public:
    std::string Translate(const std::string& from, const uint baseFrom, const uint baseTo) {
        uint powers[baseFrom] {0}; // logarithm[i] == log_{baseTo}(i)
        for (auto i = 0u, v = 1u; i < baseFrom; ++i) {
            powers[i] = v;
            if (i == v) {
                v *= baseTo;
            }
        }

        std::string baseFromMultiplier;
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
                    baseFromMultiplier.push_back(Digit2Char(reminder / power));
                    reminder %= power;
                } else {
                    break;
                }
            }
        }

        std::string res;
        auto& multiplier = Buf[0] = "1";

        for (auto it = from.rbegin(); it != from.rend(); ++it) {
            if (auto rem = Char2Digit(*it, baseFrom)) {
                Buf[1].clear();
                for (auto power = powers[rem]; power; power /= baseTo) {
                    Buf[1].push_back(Digit2Char(rem / power));
                    rem %= power;
                }
            } else {
                Buf[1].assign(1, '0');
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

            if (res.empty()) {
                std::swap(res, Buf[2]);
            } else {
                Sum(res, Buf[2], baseTo, Buf[1]);
                std::swap(res, Buf[1]);
            }
        }

        RemoveLeadingZeros(res);
        return res;
    }

private:
    static constexpr uint Char2Digit(char c, const uint base) noexcept {
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

    static constexpr char Digit2Char(const uint num) noexcept {
        if (num < 10) {
            return '0' + num;
        } else {
            return 'a' + (num - 10);
        }
    }

    void Sum(const std::string& first, const std::string& second, const uint base, std::string& res) {
        res.clear();
        res.reserve(first.size() + second.size());

        uint carry = 0;

        auto f = first.rbegin(), s = second.rbegin();
        const auto doIter = [&] (auto& v, auto& iter, const auto& str) {
            if (iter != str.rend()) {
                v += Char2Digit(*iter, base);
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
            res.push_back(Digit2Char(v % base));
        };

        if (carry) {
            assert(carry < base);
            res.push_back(Digit2Char(carry));
        }

        std::reverse(res.begin(), res.end());
    }

    void Multiply(const std::string& str, const char ch, const uint base, std::string& res, const uint padding = 0) {
        res.clear();
        res.reserve(str.size() + 1);

        const auto m = Char2Digit(ch, base);
        uint carry = 0;
        for (auto it = str.rbegin(); it != str.rend(); ++it) {
            carry += Char2Digit(*it, base) * m;
            res.push_back(Digit2Char(carry % base));
            carry /= base;
            assert(carry < base);
        }

        if (carry) {
            assert(carry < base);
            res.push_back(Digit2Char(carry));
        }

        std::reverse(res.begin(), res.end());
        res.insert(res.end(), padding, '0');
    }

    void RemoveLeadingZeros(std::string& str) {
        const auto leadingZeros = std::find_if(str.begin(), str.end(), [&] (char c) {
            return c != '0';
        });
        if (leadingZeros == str.end()) {
            str.assign(1, '0');
        } else {
            str.erase(str.begin(), leadingZeros);
        }
    }

    void Multiply(const std::string& first, const std::string& second, const uint base, std::string& res) {
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
    std::array<std::string, 5> Buf;
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
