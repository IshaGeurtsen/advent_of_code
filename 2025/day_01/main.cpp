//
// Created by Isha on 01/12/2025.
//
#include <cassert>
#include <iostream>
#include <fstream>
#include <utility>

class error final : public std::exception {
public:
    const char* message;
    error() = delete;
    explicit error(const char* message): message(message){}

    [[nodiscard]] const char* what() const noexcept override{
        return "uncaught error";
    }
};

class FileHandle final {
    std::ifstream file;
public:
    FileHandle() = delete;
    explicit FileHandle(const std::string &path) {
        file.open(path);
        if (file.fail())
            throw error("failed to open file");
    }

    ~FileHandle() {
        file.close();
    }

    template<typename T>
     FileHandle& operator>>(T& right) {
        file >> right;
        if (file.fail())
            throw error("read error");
        return *this;
    }
};

class Command final {
public:
    const char direction = '\0';
    const int step = 0;

    Command() = delete;

    explicit Command(const char direction, const int step)
    : direction(direction), step(step)
    {}

    [[nodiscard]] static Command new_command(FileHandle& handle) {
        char direction;
        int step;
        handle >> direction >> step;
        if (direction != 'R' && direction != 'L')
            throw error("custom message");
        if (step < 0 || step > 99) {
            throw std::exception();
        }
        return Command(direction, step);
    }
};

class Dial final {
    int position = 0;
public:
    void rotate(const Command& cmd) {
        if (cmd.direction)
            position += cmd.direction;
    }

    friend std::ostream& operator<<(std::ostream& os, const Dial& cmd);
};

std::ostream& operator<<(std::ostream& os, const Command& cmd) {
    return os << cmd.direction << cmd.step << '\n';
}

std::ostream& operator<<(std::ostream& os, const Dial& dial) {
    return os << dial.position;
}


int main() {
    auto file = FileHandle("c:/users/139a4/code/advent_of_code/2025/day_01/input.txt");
    auto dial = Dial();

    try {
        while (true) {
            const auto command = Command::new_command(file);
            dial.rotate(command);
        }
    } catch (error &e) {
        std::cout << "encountered error: " << e.message << std::endl;
    }

    std::cout << dial << std::endl;

    return 0;
}