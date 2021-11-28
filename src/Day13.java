import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Day13 {
    enum Direction {
        UP('^', 0, -1) {
            Direction left() {return LEFT;}
            Direction right() {return RIGHT;}
        },
        DOWN('v', 0, 1) {
            Direction left() {return RIGHT;}
            Direction right() {return LEFT;}
        },
        LEFT('<', -1, 0) {
            Direction left() {return DOWN;}
            Direction right() {return UP;}
        },
        RIGHT('>', 1, 0) {
            Direction left() {return UP;}
            Direction right() {return DOWN;}
        };

        private final char dir;
        private final int dx;
        private final int dy;

        Direction(char dir, int dx, int dy) {
            this.dir = dir;
            this.dx = dx;
            this.dy = dy;
        }

        abstract Direction left();

        abstract Direction right();

        boolean isVert() {
            return this == UP || this == DOWN;
        }

        public static Optional<Direction> ofChar(char dir) {
            return Arrays.stream(values()).filter(d -> d.dir == dir).findFirst();
        }
    }

    static class Cart {
        int x;
        int y;
        Direction dir;
        final Track track;
        int rep;

        public Cart(int x, int y, Direction direction, Track track) {
            this.x = x;
            this.y = y;
            this.dir = direction;
            this.track = track;
        }

        void move() {
            x += dir.dx;
            y += dir.dy;
        }

        void turn() {
            dir = switch (track.track[y][x]) {
                case '\\' -> dir.isVert() ? dir.left() : dir.right();
                case '/' -> dir.isVert() ? dir.right() : dir.left();
                case '+' -> switch (rep++ % 3) {
                    case 0 -> dir.left();
                    case 2 -> dir.right();
                    default -> dir;
                };
                default -> dir;
            };
        }

        int toLoc() {
            return y * track.track.length + x;
        }

        @Override
        public String toString() {
            return "Cart{" + "x=" + x + ", y=" + y + ", direction=" + dir + ", rep=" + rep + '}';
        }
    }

    static class Track {
        final char[][] track;
        final List<Cart> carts = new ArrayList<>();

        Track(Path path) throws IOException {
            var lines = Files.readAllLines(path);
            var maxLineLength = lines.stream().map(String::length).max(Integer::compareTo).orElse(0);
            int height = lines.size();
            track = new char[height][maxLineLength];
            for (int y = 0; y < height; y++) {
                var line = lines.get(y);
                for (int x = 0; x < line.length(); x++) {
                    char c = line.charAt(x);
                    track[y][x] = c;
                    int xx = x;
                    int yy = y;
                    Direction.ofChar(c).ifPresent(d -> carts.add(new Cart(xx, yy, d, this)));
                }
            }
        }

        void moveCarts() {
            sortCarts();
            for (Cart cart : List.copyOf(carts)) {
                cart.move();
                cart.turn();
                removeCrashed();
            }
        }

        private void sortCarts() {
            carts.sort(Comparator.comparingInt(c -> ((Cart) c).y).thenComparingInt(c -> ((Cart) c).x));
        }

        void removeCrashed() {
            Map<Integer, List<Cart>> frequencies = carts.stream().collect(Collectors.groupingBy(Cart::toLoc));
            frequencies.values().forEach(crashed -> {
                if (crashed.size() != 1) {
                    System.out.format("crash at (%d,%d)\n", crashed.get(0).x, crashed.get(0).y);
                    carts.removeAll(crashed);
                }
            });
        }
    }

    /*
     * crash at (79,128)
     * ...
     * Cart{x=3, y=42, direction=DOWN, rep=9687}
     */
    public static void main(String[] args) throws IOException {
        var track = new Track(Path.of("src/day13-input.txt"));
        while (track.carts.size() > 1) {
            track.moveCarts();
        }
        System.out.println(track.carts.get(0));
    }
}
