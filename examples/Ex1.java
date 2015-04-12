/*
 *
 * v1: 1, field1
 * v2: 1, field1, 2, field2
 * v3: 1, field1, 2, field2, 3, field3
 * v4: 1, field1, 3, field3
 */

public class DataStruct {
    int field1;
    int field3;
}

//...

public DataStruct read(int[] in) {
    DataStruct result = new DataStruct();
    result.field1 = in[1];

    if (in.length > 2 && in[2] == 3) {
        result.field3 = in[3];
    } else if (in.length > 4 && in[4] == 3) {
        result.field3 = in[5];
    } else {
        result.field3 = -1;
    }

    return result;
}