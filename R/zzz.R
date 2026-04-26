## Package load hooks.

.onLoad <- function(libname, pkgname) {
    S7::methods_register()

    ns <- parent.env(environment())

    registerS3method(
        "Ops", "homomorpheR::PaillierCiphertext",
        .homomorpheR_Ops_handler, envir = ns)
    registerS3method(
        "Ops", "homomorpheR::PaillierEncryptedReal",
        .homomorpheR_real_Ops_handler, envir = ns)

    if (getRversion() >= "4.3.0") {
        registerS3method(
            "chooseOpsMethod", "homomorpheR::PaillierCiphertext",
            .homomorpheR_chooseOpsMethod, envir = ns)
        registerS3method(
            "chooseOpsMethod", "homomorpheR::PaillierEncryptedReal",
            .homomorpheR_real_chooseOpsMethod, envir = ns)
    }
}
