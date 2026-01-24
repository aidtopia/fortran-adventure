#ifndef AID_FORTRAN_BASICSTATEMENT_H
#define AID_FORTRAN_BASICSTATEMENT_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace aid::fortran {

using statement_number_t = std::uint_fast32_t;
inline statement_number_t constexpr no_statement_number =
    static_cast<statement_number_t>(-1);

class unit;  // forward reference to avoid circular dependency

class basic_statement {
    public:
        basic_statement() = default;
        explicit basic_statement(statement_number_t number) :
            m_number(number) {}
        virtual ~basic_statement() {};

        void set_statement_number(statement_number_t number) {
            m_number = number;
        }
        statement_number_t get_statement_number() const {
            return m_number;
        }

        std::string generate(unit const &u) const;

        // Uses the unit interface to mark symbols as referenced.
        void mark_reachable(unit &u);

        bool is_reachable() const { return m_reachable; }

        // may_proceed() means the execution could possibly proceed to the next
        // statement after executing this one.  Essentially, it's false for
        // unconditional GOTO, numeric IF, RETURN, and STOP.
        virtual bool may_proceed() const { return true; }

    private:
        virtual std::string do_generate(unit const &) const;
        virtual void do_mark_reachable(unit &) const;

        statement_number_t m_number = no_statement_number;
        bool m_reachable = false;
};

// Using shared_ptr instead of unique_ptr because units, which contain
// statement_blocks, are copied.
using statement_t = std::shared_ptr<basic_statement>;
using statement_block = std::vector<statement_t>;

}

#endif
