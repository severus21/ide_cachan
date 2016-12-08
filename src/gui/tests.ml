open OUnit2
open GuiFindDialog

let test_same_letter _ =
    assert_equal (is_subword "a" "a") true
;;

let test_same_word _ =
    let word = "aefhlVjgaeHsjlkf" in
    assert_equal (is_subword word word) true
;;

let test_different_letter _ =
    assert_equal (is_subword "a" "b") false
;;

let test_subword1 _ =
    assert_equal (is_subword "a" "aaaa") true
;;

let test_subword2 _ =
    assert_equal (is_subword "b" "aaba") true
;;

let test_subword3 _ =
    assert_equal (is_subword "ba" "abba") true
;;

let suite =
    "gui_find_dialog_suite">::: [
        "same_letter"     >:: test_same_letter;
        "same_word"       >:: test_same_word;
        "different_letter">:: test_different_letter;
        "subword1"        >:: test_subword1;
        "subword2"        >:: test_subword2;
        "subword3"        >:: test_subword3
    ]
;;

let tests () = suite;;
