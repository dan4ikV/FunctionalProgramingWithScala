public class Task1 {
    public static void main(String[] args) {
            int[] arr = new int[]{1, 2, 3 ,4, 5};
            System.out.println(sum(arr));
    }

    public static int sum (int[] arr){
        int sum = 0;
        for(int i : arr){
            sum += arr[i];
        }
        return sum;
    }
}
