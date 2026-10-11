// Build:  g++ -std=c++17 -O2 graphing_calculator.cpp -o graphcalc
// Run:    ./graphcalc

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

// ---------------------------------------------------------------------------
// Expression AST
// ---------------------------------------------------------------------------
struct Node {
    enum Type { Num, Var, Neg, Bin, Func } type;
    double value = 0;
    char op = 0;
    std::string fn;
    std::unique_ptr<Node> left, right;
    explicit Node(Type t) : type(t) {}
};
using NodePtr = std::unique_ptr<Node>;

static double applyFunc(const std::string& f, double a) {
    if (f == "sin") return std::sin(a);
    if (f == "cos") return std::cos(a);
    if (f == "tan") return std::tan(a);
    if (f == "asin") return std::asin(a);
    if (f == "acos") return std::acos(a);
    if (f == "atan") return std::atan(a);
    if (f == "sinh") return std::sinh(a);
    if (f == "cosh") return std::cosh(a);
    if (f == "tanh") return std::tanh(a);
    if (f == "sqrt") return std::sqrt(a);
    if (f == "cbrt") return std::cbrt(a);
    if (f == "abs") return std::fabs(a);
    if (f == "exp") return std::exp(a);
    if (f == "ln") return std::log(a);
    if (f == "log") return std::log10(a);
    if (f == "floor") return std::floor(a);
    if (f == "ceil") return std::ceil(a);
    throw std::runtime_error("unknown function: " + f);
}

static bool isKnownFunc(const std::string& f) {
    static const char* names[] = {"sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh",
                                  "tanh", "sqrt", "cbrt", "abs", "exp", "ln", "log", "floor", "ceil"};
    for (auto n : names) if (f == n) return true;
    return false;
}

double evaluate(const Node* n, double x) {
    switch (n->type) {
        case Node::Num: return n->value;
        case Node::Var: return x;
        case Node::Neg: return -evaluate(n->left.get(), x);
        case Node::Func: return applyFunc(n->fn, evaluate(n->left.get(), x));
        case Node::Bin: {
            double a = evaluate(n->left.get(), x);
            double b = evaluate(n->right.get(), x);
            switch (n->op) {
                case '+': return a + b;
                case '-': return a - b;
                case '*': return a * b;
                case '/': return a / b;
                case '^': return std::pow(a, b);
            }
        }
    }
    return NAN;
}

// ---------------------------------------------------------------------------
// Recursive-descent parser
//   expr    := term (('+'|'-') term)*
//   term    := unary (('*'|'/') unary | implicit-multiplication)*
//   unary   := ('-'|'+') unary | power
//   power   := primary ('^' unary)?          (right associative)
//   primary := number | 'x' | 'pi' | 'e' | func '(' expr ')' | '(' expr ')'
// ---------------------------------------------------------------------------
class Parser {
public:
    explicit Parser(const std::string& s) : src(s) {}

    NodePtr parse() {
        NodePtr n = expr();
        skipWs();
        if (pos != src.size())
            throw std::runtime_error(std::string("unexpected '") + src[pos] + "'");
        return n;
    }

private:
    std::string src;
    size_t pos = 0;

    void skipWs() { while (pos < src.size() && std::isspace((unsigned char)src[pos])) ++pos; }
    char peek() { skipWs(); return pos < src.size() ? src[pos] : '\0'; }

    static NodePtr bin(char op, NodePtr l, NodePtr r) {
        auto n = std::make_unique<Node>(Node::Bin);
        n->op = op; n->left = std::move(l); n->right = std::move(r);
        return n;
    }

    NodePtr expr() {
        NodePtr left = term();
        while (peek() == '+' || peek() == '-') {
            char op = src[pos++];
            left = bin(op, std::move(left), term());
        }
        return left;
    }

    bool startsPrimary() {
        char c = peek();
        return std::isdigit((unsigned char)c) || c == '.' || std::isalpha((unsigned char)c) || c == '(';
    }

    NodePtr term() {
        NodePtr left = unary();
        for (;;) {
            char c = peek();
            if (c == '*' || c == '/') {
                ++pos;
                left = bin(c, std::move(left), unary());
            } else if (startsPrimary()) {              // implicit multiplication: 2x, 3(x+1), x sin(x)
                left = bin('*', std::move(left), power());
            } else break;
        }
        return left;
    }

    NodePtr unary() {
        char c = peek();
        if (c == '-') {
            ++pos;
            auto n = std::make_unique<Node>(Node::Neg);
            n->left = unary();
            return n;
        }
        if (c == '+') { ++pos; return unary(); }
        return power();
    }

    NodePtr power() {
        NodePtr base = primary();
        if (peek() == '^') {
            ++pos;
            return bin('^', std::move(base), unary());
        }
        return base;
    }

    NodePtr primary() {
        char c = peek();
        if (std::isdigit((unsigned char)c) || c == '.') {
            size_t start = pos;
            while (pos < src.size() && (std::isdigit((unsigned char)src[pos]) || src[pos] == '.')) ++pos;
            auto n = std::make_unique<Node>(Node::Num);
            try { n->value = std::stod(src.substr(start, pos - start)); }
            catch (...) { throw std::runtime_error("bad number"); }
            return n;
        }
        if (std::isalpha((unsigned char)c)) {
            size_t start = pos;
            while (pos < src.size() && std::isalpha((unsigned char)src[pos])) ++pos;
            std::string id = src.substr(start, pos - start);
            for (auto& ch : id) ch = (char)std::tolower((unsigned char)ch);

            if (id == "x") return std::make_unique<Node>(Node::Var);
            if (id == "pi" || id == "e") {
                auto n = std::make_unique<Node>(Node::Num);
                n->value = (id == "pi") ? M_PI : M_E;
                return n;
            }
            if (isKnownFunc(id)) {
                if (peek() != '(') throw std::runtime_error("expected '(' after " + id);
                ++pos;
                auto n = std::make_unique<Node>(Node::Func);
                n->fn = id;
                n->left = expr();
                if (peek() != ')') throw std::runtime_error("missing ')'");
                ++pos;
                return n;
            }
            throw std::runtime_error("unknown identifier: " + id);
        }
        if (c == '(') {
            ++pos;
            NodePtr n = expr();
            if (peek() != ')') throw std::runtime_error("missing ')'");
            ++pos;
            return n;
        }
        if (c == '\0') throw std::runtime_error("unexpected end of expression");
        throw std::runtime_error(std::string("unexpected '") + c + "'");
    }
};

// ---------------------------------------------------------------------------
// Graph state and ASCII renderer
// ---------------------------------------------------------------------------
struct Function {
    std::string text;
    NodePtr ast;
};

class Grapher {
public:
    double xmin = -10, xmax = 10, ymin = -10, ymax = 10;
    int W = 78, H = 28;
    std::vector<Function> funcs;

    void add(const std::string& text) {
        funcs.push_back({text, Parser(text).parse()});
    }

    void zoom(double factor) {
        double cx = (xmin + xmax) / 2, cy = (ymin + ymax) / 2;
        double hx = (xmax - xmin) / 2 * factor, hy = (ymax - ymin) / 2 * factor;
        xmin = cx - hx; xmax = cx + hx;
        ymin = cy - hy; ymax = cy + hy;
    }

    void pan(double dx, double dy) {
        double sx = (xmax - xmin) * dx, sy = (ymax - ymin) * dy;
        xmin += sx; xmax += sx; ymin += sy; ymax += sy;
    }

    // Choose a y-range that fits all functions across the current x-range.
    void autoY() {
        double lo = INFINITY, hi = -INFINITY;
        for (auto& f : funcs)
            for (int c = 0; c < W * 4; ++c) {
                double x = xmin + (xmax - xmin) * c / (W * 4 - 1);
                double y = evaluate(f.ast.get(), x);
                if (std::isfinite(y) && std::fabs(y) < 1e6) { lo = std::min(lo, y); hi = std::max(hi, y); }
            }
        if (!std::isfinite(lo) || !std::isfinite(hi)) return;
        if (hi - lo < 1e-9) { lo -= 1; hi += 1; }
        double pad = (hi - lo) * 0.08;
        ymin = lo - pad; ymax = hi + pad;
    }

    void draw() const {
        std::vector<std::string> grid(H, std::string(W, ' '));

        // grid axes
        int axisRow = (ymin <= 0 && ymax >= 0) ? yToRow(0) : -1;
        int axisCol = (xmin <= 0 && xmax >= 0) ? xToCol(0) : -1;
        if (axisRow >= 0) for (int c = 0; c < W; ++c) grid[axisRow][c] = '-';
        if (axisCol >= 0) for (int r = 0; r < H; ++r) grid[r][axisCol] = '|';
        if (axisRow >= 0 && axisCol >= 0) grid[axisRow][axisCol] = '+';

        // curves
        static const char marks[] = "*ox#@%";
        for (size_t i = 0; i < funcs.size(); ++i) {
            char mark = marks[i % (sizeof(marks) - 1)];
            int prevRow = -1;
            bool prevValid = false;
            double prevY = 0;
            for (int c = 0; c < W; ++c) {
                double x = xmin + (xmax - xmin) * c / (W - 1);
                double y = evaluate(funcs[i].ast.get(), x);
                if (!std::isfinite(y)) { prevValid = false; continue; }

                double rowF = (ymax - y) / (ymax - ymin) * (H - 1);
                int row = (int)std::lround(std::max(-1e6, std::min(1e6, rowF)));

                // connect to previous column unless it looks like an asymptote jump
                bool connect = prevValid && std::fabs(y - prevY) < (ymax - ymin) * 2;
                int from = connect ? prevRow : row;
                int lo = std::min(from, row), hi = std::max(from, row);
                lo = std::max(lo, 0); hi = std::min(hi, H - 1);
                for (int r = lo; r <= hi; ++r) grid[r][c] = mark;

                prevRow = row; prevY = y; prevValid = true;
            }
        }

        // output with simple labels
        std::cout << std::fixed << std::setprecision(2);
        for (int r = 0; r < H; ++r) {
            std::ostringstream label;
            if (r == 0) label << std::fixed << std::setprecision(2) << ymax;
            else if (r == H - 1) label << std::fixed << std::setprecision(2) << ymin;
            else if (r == axisRow) label << "0";
            std::cout << std::setw(8) << label.str() << " " << grid[r] << "\n";
        }
        std::ostringstream xl;
        xl << std::fixed << std::setprecision(2) << xmin;
        std::string left = xl.str();
        xl.str(""); xl << xmax;
        std::string right = xl.str();
        int gap = std::max(1, W - (int)left.size() - (int)right.size());
        std::cout << std::string(9, ' ') << left << std::string(gap, ' ') << right << "\n";

        for (size_t i = 0; i < funcs.size(); ++i)
            std::cout << "  [" << marks[i % (sizeof(marks) - 1)] << "] y" << i + 1 << " = " << funcs[i].text << "\n";
    }

private:
    int xToCol(double x) const { return (int)std::lround((x - xmin) / (xmax - xmin) * (W - 1)); }
    int yToRow(double y) const { return (int)std::lround((ymax - y) / (ymax - ymin) * (H - 1)); }
};

// ---------------------------------------------------------------------------
// Helpers and REPL
// ---------------------------------------------------------------------------
static std::string trim(const std::string& s) {
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a == std::string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b - a + 1);
}

static void printHelp() {
    std::cout <<
R"(Commands
  y = <expr>            add a function of x, e.g.  y = sin(x)/x
  <expr>                evaluate a number, e.g.  2*pi^2 + sqrt(9)   (x counts as 0)
  plot                  draw all functions
  list                  show functions
  remove <n>            remove function n      clear   remove all
  range a b c d         set x-min x-max y-min y-max
  auto                  fit y-range to the functions
  zoom in | zoom out    zoom by 2x
  pan l|r|u|d           shift the view by 25%
  eval <n> <x>          value of function n at x
  table <n> a b step    table of values for function n
  size <w> <h>          set plot size in characters
  help                  show this message      quit   exit

Operators: + - * / ^ ( )   implicit multiply: 2x, 3(x+1)
Functions: sin cos tan asin acos atan sinh cosh tanh sqrt cbrt abs exp ln log floor ceil
Constants: pi e
)";
}

int main() {
    Grapher g;
    std::cout << "Graphing Calculator  (type 'help' for commands)\n";
    g.add("sin(x)");
    g.draw();

    std::string line;
    while (std::cout << "\n> ", std::getline(std::cin, line)) {
        line = trim(line);
        if (line.empty()) continue;

        std::string lower = line;
        for (auto& c : lower) c = (char)std::tolower((unsigned char)c);
        std::istringstream in(lower);
        std::string cmd;
        in >> cmd;

        try {
            if (cmd == "quit" || cmd == "exit") break;
            else if (cmd == "help") printHelp();
            else if (cmd == "plot") g.draw();
            else if (cmd == "list") {
                for (size_t i = 0; i < g.funcs.size(); ++i)
                    std::cout << "y" << i + 1 << " = " << g.funcs[i].text << "\n";
                if (g.funcs.empty()) std::cout << "(no functions)\n";
            }
            else if (cmd == "clear") { g.funcs.clear(); std::cout << "cleared\n"; }
            else if (cmd == "remove") {
                size_t n; 
                if (!(in >> n) || n < 1 || n > g.funcs.size()) throw std::runtime_error("usage: remove <n>");
                g.funcs.erase(g.funcs.begin() + (n - 1));
                g.draw();
            }
            else if (cmd == "range") {
                double a, b, c, d;
                if (!(in >> a >> b >> c >> d) || a >= b || c >= d)
                    throw std::runtime_error("usage: range xmin xmax ymin ymax (min < max)");
                g.xmin = a; g.xmax = b; g.ymin = c; g.ymax = d;
                g.draw();
            }
            else if (cmd == "auto") { g.autoY(); g.draw(); }
            else if (cmd == "zoom") {
                std::string d; in >> d;
                if (d == "in") g.zoom(0.5);
                else if (d == "out") g.zoom(2.0);
                else throw std::runtime_error("usage: zoom in|out");
                g.draw();
            }
            else if (cmd == "pan") {
                std::string d; in >> d;
                if (d == "l") g.pan(-0.25, 0);
                else if (d == "r") g.pan(0.25, 0);
                else if (d == "u") g.pan(0, 0.25);
                else if (d == "d") g.pan(0, -0.25);
                else throw std::runtime_error("usage: pan l|r|u|d");
                g.draw();
            }
            else if (cmd == "size") {
                int w, h;
                if (!(in >> w >> h) || w < 20 || h < 8 || w > 200 || h > 80)
                    throw std::runtime_error("usage: size <20-200> <8-80>");
                g.W = w; g.H = h;
                g.draw();
            }
            else if (cmd == "eval") {
                size_t n; double x;
                if (!(in >> n >> x) || n < 1 || n > g.funcs.size()) throw std::runtime_error("usage: eval <n> <x>");
                std::cout << "y" << n << "(" << x << ") = " << std::setprecision(10)
                          << evaluate(g.funcs[n - 1].ast.get(), x) << "\n";
            }
            else if (cmd == "table") {
                size_t n; double a, b, step;
                if (!(in >> n >> a >> b >> step) || n < 1 || n > g.funcs.size() || step <= 0 || a > b)
                    throw std::runtime_error("usage: table <n> start end step");
                std::cout << std::setw(12) << "x" << std::setw(16) << "y" << "\n";
                int count = 0;
                for (double x = a; x <= b + 1e-12 && count < 200; x += step, ++count)
                    std::cout << std::setw(12) << std::setprecision(6) << x << std::setw(16) << std::setprecision(8)
                              << evaluate(g.funcs[n - 1].ast.get(), x) << "\n";
            }
            else if (lower.rfind("y", 0) == 0 && lower.find('=') != std::string::npos) {
                // "y = ..." or "y1 = ..."
                std::string expr = trim(line.substr(line.find('=') + 1));
                g.add(expr);
                g.draw();
            }
            else {
                // plain calculation
                NodePtr ast = Parser(line).parse();
                std::cout << "= " << std::setprecision(12) << evaluate(ast.get(), 0) << "\n";
            }
        } catch (const std::exception& e) {
            std::cout << "error: " << e.what() << "\n";
        }
    }
    return 0;
}