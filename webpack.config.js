var path = require("path");
var HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
    entry: {
        app: [
            "./src/static/index.js",
        ]
    },

    output: {
        path: path.resolve(__dirname + "/dist"),
        filename: "[name].js",
    },

    resolve: {
        extensions: [".js", ".elm"],
        modules: ["node_modules"],
    },

    module: {
        rules: [{
            test: /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            use: [{
                loader: "elm-webpack-loader",
                options: {
                    verbose: true,
                    warn: true,
                    debug: true,
                },
            }],
        }],

        noParse: /\.elm$/
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: "src/static/index.html",
            inject: "body",
            filename: "index.html",
        }),
    ],

    devServer: {
        inline: true,
        historyApiFallback: true,
        contentBase: "./src",
        hot: true,
        stats: {
            colors: true,
        },
    },
};
