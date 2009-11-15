n_of_arrows(10).
n_of_columns(10).

cell_land([1, 1], forest).
cell_land([2, 1], forest).
cell_land([3, 1], forest).
cell_land([4, 1], forest).
cell_land([5, 1], forest).
cell_land([6, 1], forest).
cell_land([7, 1], forest).
cell_land([8, 1], forest).
cell_land([9, 1], forest).
cell_land([10, 1], forest).
cell_land([1, 2], forest).
cell_land([2, 2], plain).
cell_land([3, 2], water).
cell_land([4, 2], plain).
cell_land([5, 2], mountain).
cell_land([6, 2], plain).
cell_land([7, 2], plain).
cell_land([8, 2], plain).
cell_land([9, 2], forest).
cell_land([10, 2], forest).
cell_land([1, 3], forest).
cell_land([2, 3], water).
cell_land([3, 3], mountain).
cell_land([4, 3], plain).
cell_land([5, 3], plain).
cell_land([6, 3], plain).
cell_land([7, 3], mountain).
cell_land([8, 3], mountain).
cell_land([9, 3], forest).
cell_land([10, 3], forest).
cell_land([1, 4], forest).
cell_land([2, 4], water).
cell_land([3, 4], plain).
cell_land([4, 4], plain).
cell_land([5, 4], mountain).
cell_land([6, 4], plain).
cell_land([7, 4], plain).
cell_land([8, 4], plain).
cell_land([9, 4], plain).
cell_land([10, 4], forest).
cell_land([1, 5], forest).
cell_land([2, 5], forest).
cell_land([3, 5], plain).
cell_land([4, 5], mountain).
cell_land([5, 5], mountain).
cell_land([6, 5], mountain).
cell_land([7, 5], mountain).
cell_land([8, 5], mountain).
cell_land([9, 5], plain).
cell_land([10, 5], forest).
cell_land([1, 6], forest).
cell_land([2, 6], plain).
cell_land([3, 6], plain).
cell_land([4, 6], mountain).
cell_land([5, 6], plain).
cell_land([6, 6], plain).
cell_land([7, 6], plain).
cell_land([8, 6], mountain).
cell_land([9, 6], mountain).
cell_land([10, 6], forest).
cell_land([1, 7], forest).
cell_land([2, 7], plain).
cell_land([3, 7], plain).
cell_land([4, 7], mountain).
cell_land([5, 7], mountain).
cell_land([6, 7], plain).
cell_land([7, 7], mountain).
cell_land([8, 7], plain).
cell_land([9, 7], mountain).
cell_land([10, 7], forest).
cell_land([1, 8], forest).
cell_land([2, 8], water).
cell_land([3, 8], water).
cell_land([4, 8], plain).
cell_land([5, 8], plain).
cell_land([6, 8], plain).
cell_land([7, 8], mountain).
cell_land([8, 8], mountain).
cell_land([9, 8], plain).
cell_land([10, 8], forest).
cell_land([1, 9], forest).
cell_land([2, 9], water).
cell_land([3, 9], plain).
cell_land([4, 9], mountain).
cell_land([5, 9], mountain).
cell_land([6, 9], plain).
cell_land([7, 9], water).
cell_land([8, 9], plain).
cell_land([9, 9], mountain).
cell_land([10, 9], forest).
cell_land([1, 10], forest).
cell_land([2, 10], forest).
cell_land([3, 10], forest).
cell_land([4, 10], forest).
cell_land([5, 10], forest).
cell_land([6, 10], forest).
cell_land([7, 10], forest).
cell_land([8, 10], forest).
cell_land([9, 10], forest).
cell_land([10, 10], forest).

:- dynamic object_at/2.
object_at([treasure, t5, [[val, 349]]], [4, 8]).
object_at([treasure, t4, [[val, 258]]], [6, 4]).
object_at([treasure, t3, [[val, 58]]], [7, 7]).
object_at([treasure, t2, [[val, 220]]], [5, 3]).
object_at([treasure, t1, [[val, 300]]], [4, 4]).

building(hostel, h5, [3, 6], []).
building(hostel, h4, [2, 6], []).
building(hostel, h3, [2, 7], []).
building(hostel, h2, [8, 9], []).
building(hostel, h1, [2, 2], []).
