% Game settings

time_to_think(1).

vision_length(3).

mountain_resting_time(1).

unconscious_time(5).

climbing_time(1).

wake_up_stamina(20).


fight_skill_reward(1).


registration_handler_freq(2).

dice_sides(10).

hostel_recovery_rate(1).

max_stamina(50).

initial_fight_skill(100).

forbidden_entry_time(FET):- max_stamina(MS),
                            FET is round(MS/3).
                            
stamina_cost(attack, 1).
stamina_cost(turn, 1).
stamina_cost(move_fwd_plain, 1).
stamina_cost(move_fwd_mountain, 2).
stamina_cost(pickup, 0).
stamina_cost(drop, 0).