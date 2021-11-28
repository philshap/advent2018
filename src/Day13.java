import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Day13 {
    enum Direction {
        UP('^', -1, 0) {
            Direction left() {return LEFT;}
            Direction right() {return RIGHT;}
        },
        DOWN('v', 1, 0) {
            Direction left() {return RIGHT;}
            Direction right() {return LEFT;}
        },
        LEFT('<', 0, -1) {
            Direction left() {return DOWN;}
            Direction right() {return UP;}
        },
        RIGHT('>', 0, 1) {
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
        boolean isCrashed;
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

        boolean isActive() {
            return !isCrashed;
        }

        int toLoc() {
            return x * track.track.length + y;
        }

        public void crash() {
            isCrashed = true;
        }

        @Override
        public String toString() {
            return "Cart{" + "x=" + x + ", y=" + y + ", direction=" + dir + ", isCrashed=" + isCrashed + '}';
        }
    }

    static class Track {
        final char[][] track;
        final List<Cart> carts;
        boolean crashYet;

        Track(Path path) throws IOException {
            var lines = Files.readAllLines(path);
            track = new char[lines.size()][lines.get(0).length()];
            for (int y = 0; y < lines.size(); y++) {
                for (int x = 0; x < lines.get(y).length(); x++) {
                    track[y][x] = lines.get(y).charAt(x);
                }
            }
            carts = findCarts();
        }

        List<Cart> findCarts() {
            List<Cart> carts = new ArrayList<>();
            for (int y = 0; y < track.length; y++) {
                for (int x = 0; x < track[y].length; x++) {
                    int xx = x;
                    int yy = y;
                    Direction.ofChar(track[y][x]).ifPresent(d -> carts.add(new Cart(xx, yy, d, this)));
                }
            }
            return carts;
        }

        void moveCarts() {
            sortCarts();
            System.out.println(carts);
            for (Cart cart : activeCarts()) {
                cart.move();
                cart.turn();
                markCrashed();
                if (cart.isCrashed && !crashYet) {
                    System.out.println("first crash at " + cart);
                    crashYet = true;
                }
            }
        }

        private void sortCarts() {
            carts.sort(Comparator.comparingInt(c -> ((Cart) c).y).thenComparingInt(c -> ((Cart) c).x));
        }

        void markCrashed() {
            Map<Integer, List<Cart>> frequencies = activeCarts().stream().collect(Collectors.groupingBy(Cart::toLoc));
            frequencies.values().forEach(carts -> {
                if (carts.size() != 1) {
                    carts.forEach(Cart::crash);
                }
            });
        }

        List<Cart> activeCarts() {
            return carts.stream().filter(Cart::isActive).collect(Collectors.toList());
        }
    }

    public static void main(String[] args) throws IOException {
        var track = new Track(Path.of("src/day13-input-small.txt"));
        while (track.activeCarts().size() > 1) {
            track.moveCarts();
        }
        System.out.println(track.carts);
    }
}
