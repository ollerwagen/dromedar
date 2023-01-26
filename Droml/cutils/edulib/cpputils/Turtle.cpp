#include <cmath>
#include <algorithm>

#include "SFML/System/Time.hpp"
#include "SFML/Graphics.hpp"

#include "gc.h"

#include "Turtle.h"

constexpr int WINDOWSIZE = 600;

sf::RenderWindow *window = nullptr;
sf::VertexArray  *pixels = nullptr;

sf::Texture      *turt_t = nullptr;
sf::Sprite       *turtle = nullptr;


constexpr float TURT_SPEED = 100.0f;
constexpr float ANGL_SPEED = 90.0f;

bool is_pen_dn;

constexpr int   FPS        = 60;
constexpr float SPF        = 1.0f / static_cast<float>(FPS);

const sf::Time SLEEP_TIME = sf::seconds(SPF);


static void error() {
    fprintf(stderr, "You have to create the turtle window with Turtle.make() first!\n");
    exit(1);
}

#define NULLCHECK if (window == nullptr) error()


static void draw() {
    window->clear();
    window->draw(*pixels);
    window->draw(*turtle);
    window->display();
}


extern "C" {

    void _cpputils_Turtle$make() {
        printf("Turtle.make()\n");

        _removeref((ptr) window);
        window = (sf::RenderWindow*) _allocate(sizeof(sf::RenderWindow));
        new (window) sf::RenderWindow(sf::VideoMode(600, 600), "Turtle", sf::Style::Close);
        window->clear();

        _removeref((ptr) pixels);
        pixels = (sf::VertexArray*) _allocate(sizeof(sf::VertexArray));
        new (pixels) sf::VertexArray(sf::PrimitiveType::Points, WINDOWSIZE * WINDOWSIZE);
        for (int i = 0; i < WINDOWSIZE; i++) {
            for (int j = 0; j < WINDOWSIZE; j++) {
                (*pixels)[i * WINDOWSIZE + j].color    = sf::Color::White;
                (*pixels)[i * WINDOWSIZE + j].position = sf::Vector2f(i, j);
            }
        }

        std::string str = __FILE__;
        str = str.substr(0, str.find_last_of('/'));
        str += "/../data/Turtle.png";

        _removeref((ptr) turt_t);
        turt_t = (sf::Texture*) _allocate(sizeof(sf::Texture));
        turt_t->loadFromFile(str);

        _removeref((ptr) turtle);
        turtle = (sf::Sprite*) _allocate(sizeof(sf::Sprite));
        new (turtle) sf::Sprite();
        turtle->setTexture(*turt_t);
        turtle->setPosition(WINDOWSIZE / 2, WINDOWSIZE / 2);
        turtle->setOrigin(
            turtle->getLocalBounds().left + turtle->getLocalBounds().width / 2.0f,
            turtle->getLocalBounds().top + turtle->getLocalBounds().height / 2.0f);

        is_pen_dn = true;

        draw();
    }

    void _cpputils_Turtle$close() {
        printf("Turtle.close()\n");
        NULLCHECK;

        sf::Clock clock;

        while (window->isOpen()) {
            sf::sleep(SLEEP_TIME - clock.restart());

            sf::Event event;
            while (window->pollEvent(event)) {
                switch (event.type) {
                    case sf::Event::EventType::Closed:
                        window->close();
                        break;
                    default:
                        break;
                }
            }
        }

        _removeref((ptr) window);
        window = nullptr;

        _removeref((ptr) pixels);
        _removeref((ptr) turt_t);
        _removeref((ptr) turtle);
    }

    void _cpputils_Turtle$fd(double steps_) {
        printf("Turtle.fd(%lf)\n", steps_);
        NULLCHECK;

        float steps = steps_;

        float angle = turtle->getRotation() / 180.f * M_PI;
        float x_step = std::sin(angle);
        float y_step = -std::cos(angle);

        sf::Clock clock;

        while (steps > 0) {
            sf::sleep(SLEEP_TIME - clock.restart());

            float next_steps = std::min(TURT_SPEED / FPS, steps);

            if (is_pen_dn) {
                for (int i = 0; i < next_steps; i++) {
                    int x_index = turtle->getPosition().x + i * x_step, y_index = turtle->getPosition().y + i * y_step;
                    if (0 <= x_index && x_index < WINDOWSIZE && 0 <= y_index && y_index < WINDOWSIZE) {
                        (*pixels)[x_index * WINDOWSIZE + y_index].color = sf::Color::Black;
                    }
                }
            }

            turtle->setPosition(turtle->getPosition().x + next_steps * x_step, turtle->getPosition().y + next_steps * y_step);
            steps = steps - next_steps;

            draw();
        }
    }

    void _cpputils_Turtle$rt(double angle_) {
        printf("Turtle.rt(%lf)\n", angle_);
        NULLCHECK;

        float angle = angle_;

        float step_angle = std::abs(angle);
        bool right = angle > 0.0f;

        sf::Clock clock;
        while (step_angle > 0) {
            sf::sleep(SLEEP_TIME - clock.restart());

            float next_rotation = std::min(ANGL_SPEED / FPS, step_angle);
            turtle->setRotation(turtle->getRotation() + (right ? next_rotation : -next_rotation));

            step_angle -= std::abs(next_rotation);

            draw();
        }
    }

    void _cpputils_Turtle$pen(bool pen) {
        is_pen_dn = pen;
    }
    
}