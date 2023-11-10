package games.gameOfFifteen

import kotlin.math.max
import kotlin.math.min

/*
 * This function should return the parity of the permutation.
 * true - the permutation is even
 * false - the permutation is odd
 * https://en.wikipedia.org/wiki/Parity_of_a_permutation

 * If the game of fifteen is started with the wrong parity, you can't get the correct result
 *   (numbers sorted in the right order, empty cell at last).
 * Thus the initial permutation should be correct.
 */
fun isEven(permutation: List<Int>): Boolean {
    fun get(map: Map<Int, Int>, value: Int): Int {
        return if (!map.containsKey(value)) {
            value
        } else {
            val mapped = map[value]!!
            if (mapped == value)
                value
            else
                get(map, mapped)
        }
    }
    fun union(map: Map<Int, Int>, value1: Int, value2: Int): Map<Int, Int> {
        val minValue = min(value1, value2)
        val maxValue = max(value1, value2)
        return if (!map.containsKey(minValue) && !map.containsKey(maxValue)) {
            map + (maxValue to minValue)
        } else if (!map.containsKey(minValue) && map.containsKey(maxValue)) {
            val curMapped = get(map, maxValue)
            if (curMapped < minValue) {
                map + (minValue to curMapped)
            } else {
                map + (curMapped to minValue)
            }
        } else if (map.containsKey(minValue) && !map.containsKey(maxValue)) {
            val curMapped = get(map, minValue)
            map + (maxValue to curMapped)
        } else {
            val mappedMin = get(map, minValue)
            val mappedMax = get(map, maxValue)
            map + (max(mappedMin, mappedMax) to min(mappedMin, mappedMax))
        }
    }
    val min = permutation.min()
    val permGroup = permutation
        .asSequence()
        .withIndex()
        .map{pair ->
            if (min == 0) {
            pair.index to pair.value
        } else {
            pair.index + 1 to pair.value
        }}
        .fold(mapOf<Int, Int>()) { res, pair ->
            union(res, pair.first, pair.second)
        }
    val grouped = permGroup
        .asSequence()
        .fold(mapOf<Int, Set<Int>>()) { map, entry ->
            if (map.containsKey(entry.value)) {
                map + (entry.value to map[entry.value]!!.plusElement(entry.key) )
            } else {
                map + (entry.value to setOf(entry.value, entry.key))
            }
        }
    val parity = grouped
        .values
        .fold(0) {count, group ->
            if ((group.size - 1) % 2 == 0) {
                count
            } else {
                count + 1
            }
        }
    //println("xxxxxxxxxxxx " + permGroup + "/" + grouped + "/" + parity)
    return parity % 2 == 0
}