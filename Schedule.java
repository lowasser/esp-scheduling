import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;

public class Schedule {
	Map<Integer, Room> rooms;
	List<Room> roomList = new ArrayList<Room>();
	int numBlocks;
	Map<Integer, Teacher> teachers;
	Map<Integer, Class> classes;
	int totalBadness;
	int annealSteps = 0;
	static final Random myRand = new Random(System.currentTimeMillis());

	public static void main(String[] args) throws IOException {
		Schedule sched = new Schedule(new Scanner(new BufferedReader(
				new InputStreamReader(System.in))), Integer.parseInt(args[0]));
		// System.err.println("Total badness: " + sched.totalBadness);
		System.out.println(sched);
	}

	public Schedule(Scanner sc, int seconds) {
		this.numBlocks = sc.nextInt();
		int numRooms = sc.nextInt();
		this.rooms = new HashMap<Integer, Room>(numRooms);
		int numTeachers = sc.nextInt();
		this.teachers = new HashMap<Integer, Teacher>(numTeachers);
		int numClasses = sc.nextInt();
		this.classes = new HashMap<Integer, Class>(numClasses);
		int numSections = sc.nextInt();
		// rooms first
		readRooms(sc, numRooms);
		readTeachers(sc, numTeachers);
		readClasses(sc, numClasses);
		readSections(sc, numSections);
		initializeBadness();
		System.err.println("Initial badness: " + totalBadness);
		for (long start = System.currentTimeMillis(); System
				.currentTimeMillis()
				- start < 1000 * seconds;)
			anneal();
		System.err.println("Final badness: " + totalBadness);
		System.err.println("Number of iterations: " + annealSteps);
	}

	private void checkValid() {
		for (Room r : roomList) {
			for (Section s : r.roomAssignments) {
				if (s == null)
					continue;
				if (s.myRoom != r) {
					System.err.println("SUPER BAD FAIL" + s + r);
				}
			}
		}
	}

	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		for (Class c : classes.values()) {
			for (Section s : c.sections) {
				buf.append(c.cId);
				buf.append(' ');
				/*
				 * buf.append(s.secNum); buf.append(' ');
				 */
				if(s == null || s.myRoom == null)
					continue;
				buf.append(s.myRoom.roomNumber);
				buf.append(' ');
				buf.append(s.start);
				buf.append('\n');
			}
		}
		return buf.toString();
	}

	private void initializeBadness() {
		totalBadness = 0;
		for (Room r : rooms.values()) {
			for (Section s : r.roomAssignments) {
				if (s == null)
					continue;
				totalBadness += s.computeBadness0();
			}
		}
	}

	private void readSections(Scanner sc, int numSections) {
		for (int i = 0; i < numSections; i++) {
			int cId = sc.nextInt();
			int nSec = sc.nextInt();
			int rId = sc.nextInt();
			int t = sc.nextInt();
			Class c = classes.get(cId);
			// System.err.println(cId + ", " + nSec + ", " + rId + ", " + t +
			// ", "
			// + c);
			new Section(c, nSec, numBlocks, t, t + c.duration - 1, rooms.get(rId));
		}
	}

	private void readClasses(Scanner sc, int numClasses) {
		for (int i = 0; i < numClasses; i++) {
			int id = sc.nextInt();
			int duration = sc.nextInt(), numSecs = sc.nextInt(), nTeachers = sc
					.nextInt(), nRooms = sc.nextInt();
			Set<Teacher> myTeachers = new LinkedHashSet<Teacher>(nTeachers);
			for (int j = 0; j < nTeachers; j++) {
				int tId = sc.nextInt();
				if (teachers.containsKey(tId))
					myTeachers.add(teachers.get(tId));
			}

			Set<Room> myRooms = new HashSet<Room>(nRooms);
			for (int j = 0; j < nRooms; j++)
				myRooms.add(rooms.get(sc.nextInt()));
			classes.put(id, new Class(id, duration, numSecs, numBlocks,
					myTeachers, myRooms));

			// System.err.println(id + ", " + duration + ", " + numSecs + ", "
			// + myTeachers + ", " + myRooms);
		}
	}

	private void readTeachers(Scanner sc, int numTeachers) {
		for (int i = 0; i < numTeachers; i++) {
			int id = sc.nextInt();
			teachers.put(id, new Teacher(id, numBlocks));
			// System.err.println("Teacher with id " + id);
		}
	}

	private void readRooms(Scanner sc, int numRooms) {
		for (int i = 0; i < numRooms; i++) {
			int id = sc.nextInt();
			Room room = new Room(id, sc.nextInt(), sc.nextInt(), sc.nextInt(),
					numBlocks);
			rooms.put(id, room);
			roomList.add(room);
			// System.err.println("Room with ID " + id);
		}
	}

	private boolean goodSwap(int bad0, int bad1) {
		double d = (double) bad1 / bad0;
		return d < 1 + myRand.nextDouble() / Math.sqrt(annealSteps);
	}

	public void anneal() {
		int t = myRand.nextInt(numBlocks);
		int i1 = myRand.nextInt(roomList.size());
		Room r1 = roomList.get(i1);
		List<Room> r2s;
		if (r1.roomAssignments[t] == null)
			r2s = roomList;
		else {
			r2s = new ArrayList<Room>(
					r1.roomAssignments[t].thisClass.compatibleRooms);
		}
		if (r2s.isEmpty() || (r2s.size() == 1 && r2s.contains(r1)))
			return;
		Room r2;
		do {
			r2 = r2s.get(myRand.nextInt(r2s.size()));
		} while (r2.equals(r1));
		// System.err.println(i1 + ", " + i2 + ", " + r1 + ", " + r2 + ", "
		// + rooms);
		swap(t, r1, r2);
		annealSteps++;
		// System.err.println(totalBadness);
		// checkValid();
	}

	private void swap(int t, Room r1, Room r2) {
		if (r1.equals(r2))
			return;
		int startSwap = startSwap(t, r1, r2), endSwap = endSwap(t, r1, r2);
		if (badSwap(r1, r2, startSwap, endSwap))
			return;
		initializeBadness();
		int tBadness = totalBadness;
		/*
		 * int[] badness1 = new int[endSwap + 1 - startSwap], badness2 = new
		 * int[endSwap + 1 - startSwap]; int tBadness = totalBadness; for (int i
		 * = startSwap; i <= endSwap; i++) { Section s1 = r1.roomAssignments[i],
		 * s2 = r2.roomAssignments[i]; badness1[i] = s1 == null ? 0 :
		 * s1.badness; badness2[i] = s2 == null ? 0 : s2.badness; }
		 */
		/*
		 * System.err.println(Arrays.toString(r1.roomAssignments));
		 * System.err.println(Arrays.toString(r2.roomAssignments));
		 * System.err.println(startSwap + ", " + endSwap);
		 */
		executeSwap(r1, r2, startSwap, endSwap);
		updateBadness(r1, startSwap, endSwap);
		updateBadness(r2, startSwap, endSwap);
		// initializeBadness();
		if (!goodSwap(tBadness, totalBadness)) {
			executeSwap(r1, r2, startSwap, endSwap);
			// initializeBadness();
			updateBadness(r1, startSwap, endSwap);
			updateBadness(r2, startSwap, endSwap);
			if (totalBadness != tBadness) {
				System.err.println("BAD BAD BAD");
			}
			// System.err.println(Arrays.toString(r1.roomAssignments));
			// System.err.println(Arrays.toString(r2.roomAssignments));
			// System.err.println(startSwap + ", " + endSwap + ", " +
			// totalBadness
			// + ", " + tBadness);
		}
	}

	private boolean badSwap(Room r1, Room r2, int startSwap, int endSwap) {
		for (int i = startSwap; i <= endSwap; i++) {
			Section s1 = r1.roomAssignments[i], s2 = r2.roomAssignments[i];
			if (s1 != null && !s1.compatible(r2))
				return false;
			if (s2 != null && !s2.compatible(r1))
				return false;
		}
		return true;
	}

	private void updateBadness(Room r, int startSwap, int endSwap) {
		for (int i = startSwap; i <= endSwap; i++)
			recomputeLocalBadness(r.roomAssignments[i]);
	}

	private void recomputeLocalBadness(Section s) {
		if (s == null)
			return;
		totalBadness -= s.badness;
		totalBadness += s.computeBadness0();
		for (Section sPr : s.prevSections) {
			totalBadness -= sPr.badness;
			totalBadness += sPr.computeBadness0();
		}
		for (Section sNx : s.nextSections) {
			totalBadness -= sNx.badness;
			totalBadness += sNx.computeBadness0();
		}
	}

	private void executeSwap(Room r1, Room r2, int startSwap, int endSwap) {
		for (int i = startSwap; i <= endSwap; i++) {
			Section s1 = r1.roomAssignments[i], s2 = r2.roomAssignments[i];
			r1.roomAssignments[i] = s2;
			r2.roomAssignments[i] = s1;
			if (s2 != null)
				s2.myRoom = r1;
			if (s1 != null)
				s1.myRoom = r2;
		}
	}

	private int startSwap(int t, Room r1, Room r2) {
		if (t == 0)
			return 0;
		int startSwap = t;
		for (boolean flag = true; flag;) {
			flag = false;
			Section s1 = r1.roomAssignments[startSwap], s2 = r2.roomAssignments[startSwap];
			if (s1 != null && s1.start < startSwap) {
				flag = true;
				startSwap = s1.start;
			}
			if (s2 != null && s2.start < startSwap) {
				flag = true;
				startSwap = s2.start;
			}
		}
		return startSwap;
	}

	private int endSwap(int t, Room r1, Room r2) {
		if (t == numBlocks - 1)
			return 0;
		int endSwap = t;
		for (boolean flag = true; flag;) {
			flag = false;
			Section s1 = r1.roomAssignments[endSwap], s2 = r2.roomAssignments[endSwap];
			if (s1 != null && s1.end > endSwap) {
				flag = true;
				endSwap = s1.end;
			}
			if (s2 != null && s2.end > endSwap) {
				flag = true;
				endSwap = s2.end;
			}
		}
		return endSwap;
	}
}
