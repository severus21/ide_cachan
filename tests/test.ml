let () =
    let tests = OUnit2.test_list [
        Plugins.Tests.unittests ();
        Core.Tests.unittests ();
        Gui.Tests.tests ()
    ] in
    OUnit2.run_test_tt_main tests
;;
