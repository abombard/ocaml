module App =
   struct
       type project = string * string * int
       let zero = ("", "", 0)
       let combine (s1, status1, grade1) (s2, status2, grade2) =
           let s = s1 ^ s2 in
           let grade = (grade1 + grade2) / 2 in
           let status = if grade > 80 then "succeed" else "failed" in
           (s, status, grade)
       let fail (s, _, _) = (s, "failed", 0)
       let success (s, _, _) = (s ,"succeed", 80)
   end
