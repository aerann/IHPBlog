module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.View.Comments.Index
import Web.View.Comments.New
import Web.View.Comments.Edit
import Web.View.Comments.Show

instance Controller CommentsController where
    action CommentsAction = do
        (commentsQ, pagination) <- query @Comment |> paginate
        comments <- commentsQ |> fetch
        render IndexView { .. }

    action NewCommentAction { postId } = do
        let comment = newRecord
        -- match the correct post id of the post you're commenting on
                    |> set #postId postId
        post <- fetch postId
        render NewView { .. }
        -- .. means take all the variables in our scope and pass it in

    action ShowCommentAction { commentId } = do
        comment <- fetch commentId
        render ShowView { .. }

    action EditCommentAction { commentId } = do
        comment <- fetch commentId
        render EditView { .. }

    action UpdateCommentAction { commentId } = do
        comment <- fetch commentId
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> render EditView { .. }
                Right comment -> do
                    comment <- comment |> updateRecord
                    setSuccessMessage "Comment updated"
                    redirectTo EditCommentAction { .. }

-- when we create a comment, first we build the comment, if the comment isn't valid, then 
-- go back to the new page by fetching the post
    action CreateCommentAction = do
        let comment = newRecord @Comment
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> do
                    post <- fetch comment.postId
                    render NewView { .. } 
                Right comment -> do
                    comment <- comment |> createRecord
                    let postId = comment.postId
                    setSuccessMessage "Comment created"
                    -- redirect the user to the post they commented on
                    redirectTo ShowPostAction { .. }

    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        deleteRecord comment
        setSuccessMessage "Comment deleted"
        redirectTo CommentsAction

buildComment comment = comment
    |> fill @["postId", "author", "body"]
