#pragma once // 确保只被 include 一次

#include "../aoc.hpp"

#include <vector>

namespace WarGrey::AoC {
    /******************************************* 声明游戏状态 ******************************************/
    enum class RPSStatus {
        SimulateTheTournament,
        TaskDone,
        MissionDone
    };

    enum class RPSShape {
        Rock = 1,
        Paper = 2,
        Scissor = 3,
        _ = 0
    };

    enum class RPSOutcome {
        Win = 6,
        Draw = 3,
        Lose = 0,

        // this is dangerous unless you really want to
        //   treat all invalid inputs as loses.  
        _ = 0
    };

    /******************************************* 声明游戏世界 ******************************************/
    class RochamboPlane : public GYDM::Plane {
    public:
        RochamboPlane() : Plane("猜拳大赛") {}
        virtual ~RochamboPlane() {}

    public:  // 覆盖游戏基本方法
        void construct(float width, float height) override;
        void load(float width, float height) override;
        void reflow(float width, float height) override;
        void update(uint64_t count, uint32_t interval, uint64_t uptime) override;

    public:
        bool can_select(GYDM::IMatter* m) override;
        void after_select(GYDM::IMatter* m, bool yes_or_no) override;

    protected:
        void on_mission_start(float width, float height) override;
        void on_motion_step(GYDM::IMatter* m, float x, float y, double xspd, double yspd, double percentage) override;
        void on_motion_complete(GYDM::IMatter* m, float x, float y, double xspd, double yspd) override;

    private:
        void on_task_start(WarGrey::AoC::RPSStatus status);
        void on_task_done();

    private:
        int round_score(GYDM::ISprite* racer, WarGrey::AoC::RPSShape op_play, WarGrey::AoC::RPSShape sf_play, WarGrey::AoC::RPSOutcome outcome);
        int round_score(GYDM::Sprite* opponent, GYDM::Sprite* self);
        void switch_play_costume(GYDM::Sprite* target, WarGrey::AoC::RPSShape play);

    private:
        void load_strategy(const std::string& pathname);

    private: // 本游戏世界中的物体
        GYDM::Linkmon* agent;
        GYDM::Labellet* titlet;
        GYDM::GridAtlas* backdrop;
        GYDM::SpriteGridSheet* tent;
        GYDM::Labellet* population;
        GYDM::Dimensionlet* guessed_score;
        GYDM::Dimensionlet* designed_score;
        GYDM::Dimensionlet* random_score;
        GYDM::Labellet* outcome_desc;
        GYDM::Tuxmon* tux;
        GYDM::SpriteGridSheet* snack;
        GYDM::Sprite* l_play;
        GYDM::Sprite* r_play;
        GYDM::Sprite* play_icons[3];
        GYDM::Labellet* play_scores[3];
        WarGrey::AoC::ElfSheet* outcome_icons[3];
        GYDM::Labellet* outcome_scores[3];
        WarGrey::AoC::ElfSheet* elves[2];
        
    private:
        GYDM::DimensionStyle style;
        WarGrey::AoC::RPSStatus status;
        std::vector<std::pair<char, char>> strategy;
        
    private: // Shared Variables
        int current_round = 0;
        int guessed_total_score;
        int designed_total_score;
        int random_total_score;

    private:
        float round_x0;
        float round_y0;
        float race_distance;
        float race_x0;
        float race_y0;

    private: // for Progress-bar
        float distance_score;
        int guessed_final_score;
        int designed_final_score;

    private:
        GYDM::IMatter* round_self = nullptr;
    };
}
