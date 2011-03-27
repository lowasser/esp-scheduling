import java.util.HashSet;
import java.util.Set;

public class Section {
	Class thisClass;
	int secNum;
	int start;
	int end;
	Set<Section> prevSections;
	Set<Section> nextSections;
	int badness;
	Room myRoom;

	public boolean compatible(Room r) {
		return thisClass.compatibleRooms.contains(r);
	}

	public Section(Class thisClass, int secNum, int numBlocks, int start, int end, Room myRoom) {
		if(myRoom == null) return;
		this.thisClass = thisClass;
		this.secNum = secNum;
		this.start = start;
		this.end = end;
		this.myRoom = myRoom;
		for (int i = start; i <= end; i++) {
			if(myRoom.roomAssignments == null)
				myRoom.roomAssignments = new Section[numBlocks];
			myRoom.roomAssignments[i] = this;
			thisClass.sectionTimes[i] = this;
		}
		thisClass.sections[secNum] = this;
		prevSections = new HashSet<Section>();
		nextSections = new HashSet<Section>();
		for (Teacher t : thisClass.teachers) {
			Section[] tClasses = t.myClasses;
			for (int i = start; i <= end; i++)
				tClasses[i] = this;
			if (start > 0) {
				Section p = tClasses[start - 1];
				if (p != null) {
					prevSections.add(p);
					p.nextSections.add(this);
				}
			}
			if (end < tClasses.length - 1) {
				Section n = tClasses[end + 1];
				if (n != null) {
					nextSections.add(n);
					n.prevSections.add(this);
				}
			}
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + secNum;
		result = prime * result
				+ ((thisClass == null) ? 0 : thisClass.hashCode());
		return result;
	}

	public int computeBadness() {
		int ans = computeBadness0();
		for (Section pr : prevSections)
			pr.computeBadness0();
		for (Section nx : nextSections)
			nx.computeBadness0();
		return ans;
	}

	public int computeBadness0() {
		int bad = 0;
		for (Section pr : prevSections)
			bad += pr.myRoom.distance(myRoom);
		for (Section nx : nextSections)
			bad += nx.myRoom.distance(myRoom);
		this.badness = bad;
		return bad;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Section))
			return false;
		Section other = (Section) obj;
		if (secNum != other.secNum)
			return false;
		if (thisClass == null) {
			if (other.thisClass != null)
				return false;
		} else if (!thisClass.equals(other.thisClass))
			return false;
		return true;
	}
}
