import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Day14 {

    static void part1(String target) {
        int elf1 = 0;
        int elf2 = 1;
        int count = 0;
        int total = Integer.parseInt(target) + 10;
        int[] recipes = new int[total + 1];
        recipes[count++] = 3;
        recipes[count++] = 7;

        while (count <= total) {
            int recipe1 = recipes[elf1];
            int recipe2 = recipes[elf2];
            int newRecipe = recipe1 + recipe2;
            if (newRecipe > 9) {
                recipes[count++] = 1;
            }
            recipes[count++] = newRecipe % 10;
            elf1 = (elf1 + recipe1 + 1) % count;
            elf2 = (elf2 + recipe2 + 1) % count;
        }
        System.out.println(Arrays.stream(Arrays.copyOfRange(recipes, total - 10, total))
                .mapToObj(Objects::toString)
                .collect(Collectors.joining()));
    }

    static void part2(String target) {
        int elf1 = 0;
        int elf2 = 1;
        int count = 0;
        int[] targetVec = new int[target.length()];
        int i = 0;
        for (String s : target.split("")) {
            targetVec[i++] = Integer.parseInt(s);
        }
        int[] recipes = new int[30000000];
        recipes[count++] = 3;
        recipes[count++] = 7;

        while (true) {
            if (count > targetVec.length &&
                    Arrays.equals(targetVec, Arrays.copyOfRange(recipes, count - targetVec.length, count))) {
                System.out.println(count - targetVec.length);
                break;
            }
            if (count > targetVec.length &&
                    Arrays.equals(targetVec, Arrays.copyOfRange(recipes, count - targetVec.length - 1, count - 1))) {
                System.out.println(count - targetVec.length - 1);
                break;
            }
            int recipe1 = recipes[elf1];
            int recipe2 = recipes[elf2];
            int newRecipe = recipe1 + recipe2;
            if (newRecipe > 9) {
                recipes[count++] = 1;
            }
            recipes[count++] = newRecipe % 10;
            elf1 = (elf1 + recipe1 + 1) % count;
            elf2 = (elf2 + recipe2 + 1) % count;
        }
    }

    static class Bench {
        long start;
        String what;
        void lap(String what) {
            if (start != 0) {
                System.out.format("elapsed: %s\n", Duration.of(System.currentTimeMillis() - start, ChronoUnit.MILLIS));
            }
            start = System.currentTimeMillis();
            this.what = what;
        }
    }

    // part2 result 771617653 too large, took 12.5s to generate

    public static void main(String[] args) {
        Bench bench = new Bench();
        bench.lap("part1");
        part1("503761");
        bench.lap("part2");
        part2("503761");
        bench.lap("");
    }
}
