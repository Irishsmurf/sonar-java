import java.util.Set;
import java.util.EnumSet;
import java.util.HashSet;
import com.google.common.collect.Sets;


class A {

  public enum COLOR {
    RED, GREEN, BLUE, ORANGE;
  }

  class SetString implements Set<String> {
  }
  class SetColor implements Set<COLOR> {
  }
  class ExtendedSet<E> implements Set<E> {
  }

  public void doSomething(Set<COLOR> param) { // compliant, we ignore parameters.
    Set<COLOR> warm = new HashSet<COLOR>(); // Noncompliant [[sc=23;ec=43]] {{Convert this Set to an EnumSet.}}
    Set foo = new HashSet();
    warm.add(COLORS.RED);
    warm.add(COLORS.ORANGE);
    SetString ss;
    SetColor sc;
    ExtendedSet<COLOR> es; // Compliant, we check only initializer.
    Set warm2 = new HashSet<COLOR>(); // Noncompliant [[sc=17;ec=37]] {{Convert this Set to an EnumSet.}}
    EnumSet<COLOR> warm3 = EnumSet.of(COLOR.RED, COLOR.ORANGE);
    Set<COLOR> warm4 = EnumSet.of(COLOR.RED, COLOR.ORANGE);
    Set<Integer> ports2 = new HashSet<>();
    Set<COLOR> ports = new HashSet<>(); // Noncompliant [[sc=24;ec=39]] {{Convert this Set to an EnumSet.}}
    SetColor ports3 = new HashSet<>();
    Set<COLOR> ports4 = Sets.immutableEnumSet(COLOR.RED); // compliant because of bad type substitution : immutable set of E is not detected as a set of enum.
  }
}
