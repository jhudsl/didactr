#' Convert Google Slides to PNGs and Upload to Drive folder
#'
#' @param id ID or URL to Google slides presentation.
#' @param folder_name folder to upload to on Google Drive.  If no folder
#' exists, then one will be created.  Also a `dribble` can be passed
#' @param verbose print diagnostic messages, higher numbers give more output
#' @param overwrite Overwrite the PNGs if they exist, passed to
#' \code{\link{drive_upload}}
#'
#' @return A list of results
#' @export
#'
#' @examples
#' \dontrun{
#' url = paste0("https://docs.google.com/presentation/d/",
#' "1ywZbtFacZK0UIsnt2g-sheC9du_rw_7XZ1FX4rRt27M",
#' "/export/png?",
#' "id=1ywZbtFacZK0UIsnt2g-sheC9du_rw_7XZ1FX4rRt27M&pageid=g325fd519ca_0_5")
#'
#' gs_to_drive_pngs(url)
#' }
gs_to_drive_pngs = function(
    id,
    folder_name = "leanpub_pngs",
    verbose = TRUE,
    overwrite = TRUE) {
    id = didactr::get_slide_id(url)
    # pdf_file = ariExtra::download_gs_file(id = path, out_type = "pdf")
    result = didactr::gs_convert(
        id, PPTX = FALSE,
        use_gs_pngs = FALSE, use_gs_ids = TRUE)

    images = file.path(dirname(result$images),
                       paste0("id=", id, "&page_id=", basename(result$images)))
    file.rename(result$images, images)

    if (is_dribble(folder_name)) {
        trans_fol = folder_name
    } else {
        # folder_name = id
        trans_fol = googledrive::drive_find(
            pattern = folder_name,
            type = "folder", n_max = 100,
            verbose = TRUE)
        if (nrow(trans_fol) == 0) {
            trans_fol = googledrive::drive_mkdir(
                folder_name,
                verbose = verbose)
        }
        if (nrow(trans_fol) > 1) {
            warning("Multiple folders found, choosing first one")
            print(trans_fol[1,])
        }
        trans_fol = trans_fol[1,]
    }

    output = lapply(images, function(image) {
        googledrive::drive_upload(
            media = image,
            path = trans_fol,
            name = basename(image),
            overwrite = overwrite,
            verbose = verbose > 1
        )
    })
    result$images = images
    output = do.call(rbind, output)
    result$upload_output = output
    result
}
